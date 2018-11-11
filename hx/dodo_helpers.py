import os
import sys
import re
import json
import zipfile
import shutil
import tempfile
import subprocess
import uuid
import pystache

from datetime import datetime
from doit.action import CmdAction
from pathlib import *
from distutils.version import LooseVersion

def get_releasename():
    with os.popen('git describe') as f:
        return f.read().strip()

def glob1(path,wildcard):
    """
    Perform a globbing operation and confirm we get exactly one value
    """
    paths = list(path.glob(wildcard))
    if len(paths) == 1:
        return paths[0]
    raise RuntimeError("Expected 1 path, found:" + str(paths))

def rglobfiles(path):
    """
    Enumerate all of the files recursively at a path
    """
    return [p for p in path.glob('**/*') if p.is_file()]

def zipexe(path):
    """
    Produce a ZipInfo entry that has the executable bit set
    """
    dt = datetime.now()
    zinfo = zipfile.ZipInfo(str(path),date_time=(dt.year,dt.month,dt.day,dt.hour,dt.minute,dt.second))
    zinfo.external_attr = 0o755 << 16
    return zinfo

def insertZipContents(intoZipFile, atPath, fromZipFile):
    """
    Insert the contents of `fromZipFile` into `intoZipFile` at the specified path.
    """
    for f in fromZipFile.namelist():
        if not f.endswith('/'):
            bytes = fromZipFile.read(f)
            intoZipFile.writestr(str(Path(atPath) / f),bytes)

def nvmUse(version):
    """
    Return a shell command that will set a node version using nvm
    """
    if sys.platform == 'darwin':
        return "export NVM_DIR=$HOME/.nvm && source $(brew --prefix nvm)/nvm.sh && nvm install {}".format(version)
    else:
        return "export NVM_DIR=$HOME/.nvm && source $NVM_DIR/nvm.sh && nvm install {}".format(version)

def requireFile(path):
    def action():
        if not os.path.isfile(str(path)):
            raise RuntimeError('The file {} is required'.format(path))
    return action


def substituteIntoFile(file, substitutions):
    """
    Make text substitutions into an existing file
    """
    with open(str(file)) as f:
        content = f.read()
    for fromtext,totext in substitutions:
        content = content.replace(fromtext,totext)
    with open(str(file),'w') as f:
        f.write(content)

def uploadSourceMapToRollbar(rollbartoken, codeversion, minifiedurl, sourcemapfile):
    subprocess.check_call([
        'curl', 'https://api.rollbar.com/api/1/sourcemap',
        '-F', 'access_token={}'.format(rollbartoken),
        '-F', 'version={}'.format(codeversion),
        '-F', 'minified_url={}'.format(minifiedurl),
        '-F', 'source_map=@{}'.format(str(sourcemapfile))
    ])

class DockerImageRef(object):
    """
    The name of a docker image, both locally and once pushed
    to a remote repository
    """
    def __init__(self, builddir, project, name):
        self.lname = project + '_' + name
        self.rname = project + '/' + name
        self.markerfile = MarkerFile(builddir/('.' + project + name + 'built'))


class DockerContext(object):
    """
    Class representing the files to be assembled into
    the context for a docker build operation
    """
    FILE = 'FILE'
    FILE_CONTENT = 'FILE_CONTENT'
    EXPANDED_TEMPLATE = 'EXPANDED_TEMPLATE'
    TREE = 'TREE'
    ZIPTREE = 'ZIPTREE'

    def __init__(self):
        self.items = []

    def file(self, src, dest):
        """
        Add a single file to the context
        """
        self.items.append( (DockerContext.FILE, src, dest) )

    def fileContent(self, content, dest):
        """
        Add a file to the context with the given content
        """
        self.items.append( (DockerContext.FILE_CONTENT, content, dest) )

    def expandedTemplate(self, expandedtemplate, dest):
        """
        Add a expanded mustache templated file to the context.
        """
        self.items.append( (DockerContext.EXPANDED_TEMPLATE, expandedtemplate, dest) )

    def tree(self, srcdir, destdir):
        """
        Add a recursive tree to the context
        """
        self.items.append( (DockerContext.TREE, srcdir, destdir) )

    def ziptree(self, zipfile, destdir):
        """
        Unpack a zipfile into the context
        """
        self.items.append( (DockerContext.ZIPTREE, zipfile, destdir) )

    def file_dep(self):
        files = []
        for item in self.items:
            if item[0] == DockerContext.FILE:
                files.append(item[1])
            elif item[0] == DockerContext.FILE_CONTENT:
                pass
            elif item[0] == DockerContext.EXPANDED_TEMPLATE:
                files += item[1].file_dep()
            elif item[0] == DockerContext.TREE:
                files += rglobfiles(item[1])
            elif item[0] == DockerContext.ZIPTREE:
                files.append(item[1])
            else:
                raise RuntimeError( "Unknown context type: " + item[0] )
        return files

    def copyTo(self,ctxDir):
        for item in self.items:
            if item[0] == DockerContext.FILE:
                destfile = ctxDir/item[2]
                os.makedirs( str(destfile.parent), exist_ok=True)
                shutil.copyfile(str(item[1]), str(ctxDir/item[2]))
            elif item[0] == DockerContext.FILE_CONTENT:
                destfile = ctxDir/item[2]
                os.makedirs( str(destfile.parent), exist_ok=True)
                with open(str(destfile),'w') as f:
                    f.write( str(item[1]))
            elif item[0] == DockerContext.EXPANDED_TEMPLATE:
                destfile = ctxDir/item[2]
                os.makedirs( str(destfile.parent), exist_ok=True)
                with open(destfile,'w') as f:
                    f.write(item[1].content())
            elif item[0] == DockerContext.TREE:
                shutil.copytree(str(item[1]), str(ctxDir/item[2]))
            elif item[0] == DockerContext.ZIPTREE:
                destdir = ctxDir/item[2]
                os.makedirs(str(destdir), exist_ok=True)
                subprocess.run('unzip -d {} {}'.format(destdir, item[1]), check=True, shell=True)
            else:
                raise RuntimeError( "Unknown context type: " + item[0] )

class DockerImage(object):
    """
    Manage the building of a docker image in a temporary directory
    """
    def __init__( self, name, context):
        self.name = name
        self.context = context
        self.instructions = []

    def cmd(self, instruction):
        self.instructions.append(instruction)

    def action(self): return self.createImage

    def createImage(self):
        # Create a temporary directory
        ctxdir = Path(tempfile.mkdtemp())
        print( "Building image in " + str(ctxdir) )

        # Copy in the context
        self.context.copyTo(ctxdir)

        # Write a dockerfile
        with open(str(ctxdir/'Dockerfile'), 'w') as f:
            for inst in self.instructions:
                f.write(inst + '\n')

        # Run docker to build it
        subprocess.run('cd {}; docker build -t {} .'.format(ctxdir,self.name), shell=True, check=True)

        # cleanup the tempdir
        shutil.rmtree(str(ctxdir))

def docker_aws_login_action(awsregion):
   """
   Return the shell command to login to AWS required to push
   docker images
   """
   return "eval $(aws ecr get-login --region {} | sed 's/-e none//')".format(awsregion)

def publish_image_action(imageref, releasename, registry_base):
    """
    Return the shell command to publish a docker image to a registry
    (either AWS or azure)
    """
    fields = {
        "lname" : imageref.lname,
        "rname" : imageref.rname,
        "releasename" : releasename,
        "registry_base" : registry_base
    }
    cmds = [
        "docker tag {lname}:latest {registry_base}/{rname}:latest".format(**fields),
        "docker tag {lname}:latest {registry_base}/{rname}:{releasename}".format(**fields),
        "docker push {registry_base}/{rname}:latest".format(**fields),
        "docker push {registry_base}/{rname}:{releasename}".format(**fields)
    ]
    return " && ".join(cmds)


class MarkerFile(object):
    def __init__(self, path):
        self.path = path

    def action(self): return self.writeUuid

    def writeUuid(self):
        os.makedirs(os.path.dirname(str(self.path)), exist_ok=True)
        with open(str(self.path), 'w') as f:
            f.write(str(uuid.uuid4()))

class CheckException(Exception):
    pass

def getCommandPath(command):
    for path in os.environ["PATH"].split(os.pathsep):
        path = path.strip('"')
        exe_file = os.path.join(path, command)
        if os.path.isfile(exe_file) and os.access(exe_file, os.X_OK):
            return exe_file
    raise CheckException("Unable to find {} on $PATH".format(command))

def getCommandVersion(path,versionArgs,versionRegex, check=True):
    sp = subprocess.run([path] + versionArgs, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, check=check)
    match = re.match(versionRegex, sp.stdout.decode('utf-8'), flags=re.DOTALL)
    if match:
        return match.group(1)
    raise CheckException("Unable to determine version of {}".format(path))

def checkVersions(desc, version, minVersion, maxVersion):
    if minVersion and LooseVersion(version) < LooseVersion(minVersion):
        raise CheckException("{}, but need version >= {}".format(desc,minVersion))
    if maxVersion and LooseVersion(version) >= LooseVersion(maxVersion):
        raise CheckException("{}, but need version < {}".format(desc,maxVersion))

class Check(object):
    """
    Check that a program in installed, and optionally whether it has a
    suitable version
    """
    def __init__( self, command, versionArgs, versionRegex, minVersion, maxVersion, helptext, checkVersionExitStatus=True):
        self.command = command
        self.versionArgs = versionArgs
        self.versionRegex = versionRegex
        self.minVersion = minVersion
        self.maxVersion = maxVersion
        self.helptext = helptext
        self.checkVersionExitStatus = checkVersionExitStatus

    def run(self):
        path = getCommandPath(self.command)
        version = getCommandVersion(path,self.versionArgs, self.versionRegex, self.checkVersionExitStatus)
        desc = "{} found at {} (version {})".format(self.command,path,version)
        checkVersions(desc, version, self.minVersion, self.maxVersion)
        return desc

    def help(self):
        return self.helptext

class CheckNvm(object):
    """
    Check that a nvm is installed. This needs special logic as its a shell extension
    and not a program installed on the PATH
    """
    def __init__( self, minVersion, maxVersion):
        self.minVersion = minVersion
        self.maxVersion = maxVersion

    def run(self):
        sp = subprocess.run(['/bin/bash', '-i', '-c', 'nvm --version'], stdout=subprocess.PIPE)
        if sp.returncode:
            raise CheckException("nvm not installed into shell")
        match = re.match("([0-9]+.[0-9]+.([0-9]+)?)", sp.stdout.decode('utf-8'), flags=re.DOTALL)
        if not match:
            raise CheckException("Unable to determine version of nvm")
        version =  match.group(1)
        desc = 'nvm installed (version {})'.format(version)
        checkVersions(desc, version, self.minVersion, self.maxVersion)
        return desc

    def help(self):
        return "See https://github.com/creationix/nvm#installation"

class CheckEnvVar(object):
    """
    Check that an environment variable is set
    """
    def __init__(self, varname, helptext):
        self.varname = varname
        self.helptext = helptext

    def run(self):
        try:
            value = os.environ[self.varname]
        except KeyError:
            raise CheckException("env variable {} is not set".format(self.varname))
        return "{} is set (value = {})".format(self.varname,value)

    def help(self):
        return self.helptext

RED = "\033[1;31m"
GREEN = "\033[0;32m"
RESET = "\033[0;0m"

def runChecksAction(checks):
    def run():
        for check in checks:
            try:
                message = check.run()
                print( "{}OK{}: {}".format(GREEN,RESET,message) )
            except CheckException as e:
                print( "{}FAIL{}: {}".format(RED,RESET,e) )
                for helpline in check.help().split('\n'):
                    print( "      {}".format(helpline) )
                return False
        return True
    return run

def checkBazel(minVersion=None, maxVersion=None):
    return Check(
        "bazel", ["version"], '.*Build label:\s*([0-9]+.[0-9]+.([0-9]+)?)', minVersion, maxVersion,
        helptext="See https://docs.bazel.build/versions/master/install.html")

def checkYarn(minVersion=None, maxVersion=None):
    return Check(
        "yarn", ["--version"], '\s*([0-9]+.[0-9]+.([0-9]+)?)', minVersion, maxVersion,
        helptext="See https://yarnpkg.com/lang/en/docs/install")

def checkDocker(minVersion=None, maxVersion=None):
    return Check(
        "docker", ["--version"], 'Docker version\s*([0-9]+.[0-9]+.([0-9]+)?)', minVersion, maxVersion,
        helptext="See https://docs.docker.com/engine/installation/")

def checkDockerCompose(minVersion=None, maxVersion=None):
    return Check(
        "docker-compose", ["--version"], '.*version\s*([0-9]+.[0-9]+.([0-9]+)?)', minVersion, maxVersion,
        helptext="See https://docs.docker.com/compose/install/")

def checkTerraform(minVersion=None, maxVersion=None):
    return Check(
        "terraform", ["--version"], 'Terraform\s*v([0-9]+.[0-9]+.([0-9]+)?)', minVersion, maxVersion,
        helptext="See https://www.terraform.io/intro/getting-started/install.html")

def checkHaskellStack(minVersion=None, maxVersion=None):
    return Check(
        "stack", ["--version"], 'Version\s*([0-9]+.[0-9]+.([0-9]+)?)', minVersion, maxVersion,
        helptext="See https://docs.haskellstack.org/en/stable/install_and_upgrade/")

def checkAwsCli(minVersion=None, maxVersion=None):
    return Check(
        "aws", ["--version"], '.*aws-cli/([0-9]+.[0-9]+.([0-9]+)?)', minVersion, maxVersion,
        helptext="See http://docs.aws.amazon.com/cli/latest/userguide/installing.html")

def checkAzureCli(minVersion=None, maxVersion=None):
    return Check(
        "az", ["--version"], '.*azure-cli\s*\(([0-9]+.[0-9]+.([0-9]+)?)\)', minVersion, maxVersion,
        helptext="See http://docs.aws.amazon.com/cli/latest/userguide/installing.html")

def checkDoit(minVersion=None, maxVersion=None):
    return Check(
        "doit", ["--version"], '([0-9]+.[0-9]+.([0-9]+)?)', minVersion, maxVersion,
        helptext="Install python3, then see http://pydoit.org/install.html")

def checkJsonnet(minVersion=None, maxVersion=None):
    return Check(
        "jsonnet", ["--version"], '.* v([0-9]+.[0-9]+.([0-9]+)?)', minVersion, maxVersion,
        helptext="See https://github.com/google/jsonnet",
        checkVersionExitStatus=False
    )

def checkNode(minVersion=None, maxVersion=None):
    return Check(
        "node", ["--version"], 'v([0-9]+.[0-9]+.([0-9]+)?)', minVersion, maxVersion,
        helptext="See https://nodejs.org/en/download/"
    )

def checkNvm(minVersion=None, maxVersion=None):
    return CheckNvm(minVersion, maxVersion
    )

def checkEnvVar(varname, helptext):
    return CheckEnvVar(varname, helptext)


class BazelDeployJar(object):
    """
    Class specifying a deployment jar target for bazel
    """
    def __init__(self, rootdir, dir, name):
        self.rootdir = Path(rootdir)
        self.dir = Path(dir)
        self.name = name
        self.path = self.rootdir / 'bazel-bin' / self.dir / (self.name + ".jar")

def bazel_build_deployjar(target, doc):
    """
    A doit task to run bazel to build a deployment jar. Relies on
    bazels dependency analysis for checking if task needs to be run
    """
    bazeltarget = '{}:{}.jar'.format(target.dir,target.name)
    return {
        'actions': [
            'cd {}; bazel build {}'.format(target.rootdir,bazeltarget),
        ],
        'uptodate': ['cd {}; bazel build --check_up_to_date {}'.format(target.rootdir,bazeltarget)],
        'targets' : [target.path],
        'clean' : ['cd {}; bazel clean'.format(target.rootdir)],
        'doc' : doc
    }


class ZipElement(object):
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)

class ReleaseZip(object):
    """
    helper class to build a release zip file, intended for installation with
    hx-deploy-tool
    """
    def __init__(self, releasename, zipPath, prestartCommand, startCommand, stopCommand):
        self.releasename = releasename
        self.zipPath = zipPath
        self.prestartCommand = prestartCommand
        self.startCommand = startCommand
        self.stopCommand = stopCommand
        self.elements = []

    def file(self, src, dest=None, isReleaseTemplate=None):
        if isReleaseTemplate == None:
            isReleaseTemplate = dest and str(dest).endswith('.tpl') or str(src).endswith('tpl')
        self.elements.append(ZipElement(type="file",src=src,dest=dest,isReleaseTemplate=isReleaseTemplate))

    def fileContent(self, content, dest, isReleaseTemplate=None):
        if isReleaseTemplate == None:
            isReleaseTemplate = str(dest).endswith('.tpl')
        self.elements.append(ZipElement(type="content",content=content,dest=dest,isReleaseTemplate=isReleaseTemplate))

    def expandedTemplate(self, expandedtemplate, dest, isReleaseTemplate=None):
        if isReleaseTemplate == None:
            isReleaseTemplate = str(dest).endswith('.tpl')
        self.elements.append(ZipElement(type="expandedtemplate",expandedtemplate=expandedtemplate,
                                        dest=dest,isReleaseTemplate=isReleaseTemplate))

    def file_dep(self):
        return [ze.src for ze in self.elements if ze.type == 'file']
        deps = []
        for ze in self.elements:
            if ze.type == 'file':
                deps.append(ze.src)
            elif ze.type == 'expandedtemplate':
                deps.append(ze.expandedtemplate.file_dep())
        return deps

    def action(self):
        return self.createZip


    def target(self):
        return self.zipPath

    def createZip(self):
        releasejson = {
            "prestartCommand" : self.prestartCommand,
            "startCommand" : self.startCommand,
            "stopCommand" : self.stopCommand,
            "templates" : [str(self.__destPath(ze)) for ze in self.elements if ze.isReleaseTemplate]
            }
        os.makedirs(os.path.dirname(str(self.zipPath)),exist_ok=True)
        with zipfile.ZipFile(str(self.zipPath), 'w') as zf:
            zf.writestr('release.json', json.dumps(releasejson, indent=2))
            for ze in self.elements:
                if ze.type == 'content':
                    content = ze.content
                elif ze.type == 'expandedtemplate':
                    content = ze.expandedtemplate.content()
                else:
                    with open(str(ze.src),'r') as f:
                        content = f.read()
                if ze.isReleaseTemplate:
                    content = self.__withSubstitutions(content)
                zf.writestr(self.__destPath(ze), content)


    def __destPath(self, ze):
        if ze.dest == None:
            return ze.src.name
        else:
            return ze.dest

    def __withSubstitutions(self, content):
        content = content.replace('{{RELEASE_NAME}}', self.releasename)
        return content


DOCKER_PRESTART_COMMAND="""\
#!/bin/bash
set -e

# Pull the images we need
docker-compose pull
"""

AWS_DOCKER_PRESTART_COMMAND="""\
#!/bin/bash
set -e

# docker login, so we can access images from AWS ECRs
eval $(/opt/bin/hx-deploy-tool aws-docker-login-cmd)

# and pull the ones we need
docker-compose pull
"""

def DockerReleaseZip(builddir, releasename, zipname=None, prestartCommand=AWS_DOCKER_PRESTART_COMMAND):
    if zipname == None:
        zipname = 'release-{}.zip'.format(releasename)
    releasezip = ReleaseZip(
        zipPath=builddir/zipname,
        releasename=releasename,
        prestartCommand="/bin/bash ./prestart.sh",
        startCommand="docker-compose up -d",
        stopCommand="docker-compose stop && docker-compose rm -f && docker system prune -f"
    )
    releasezip.fileContent(prestartCommand, "prestart.sh")
    return releasezip


class YarnNodeModules(object):
    """
    helper class to build/update a node_modules directory using yarn
    """

    def __init__(self, dir):
        """
        The dir parameter is the directory containing node_modules, package.json, and yarn.lock
        """
        self.dir = dir
        self.markerfile = MarkerFile(dir/'node_modules/.built')

    def task(self):
        """
        Return the doit task to update the modules
        """
        node_modules = self.dir/'node_modules'
        return {
            'doc' : 'build/update node dependencies in {}'.format(self.dir),
            'actions': [
                'cd {} && yarn'.format(self.dir),
                self.markerfile.action()
            ],
            'file_dep': [self.dir/'package.json', self.dir/'yarn.lock'],
            'targets': [self.markerfile.path],
            'clean' : ["rm -r {}".format(node_modules)]
        }

    def file_dep(self):
        return [self.markerfile.path]


class ExpandedTemplate(object):
    """
    A file expanded with moustache substitutions
    (which remembers the source path for dependency purposes)
    """
    def __init__(self, templatePath, substitutions):
        self.templatePath = templatePath
        self.substitutions = substitutions

    def content(self):
        with open(self.templatePath) as f:
            template = f.read()
        return pystache.render(template, self.substitutions)

    def file_dep(self):
        return [self.templatePath]

class UnpackedZip(object):
    """
    Helper class to work with an existing zip file unpacked into
    a temporary directory
    """
    def __init__(self, zippath):
        self.zippath = zippath

    def __enter__(self):
        # Create a temporary directory
        self.workdir = Path(tempfile.mkdtemp())

        # unpack the zip file
        zipfile.ZipFile(str(self.zippath)).extractall(str(self.workdir))

        return self.workdir

    def __exit__(self, *args):
        # cleanup the tempdir
        shutil.rmtree(str(self.workdir))

def cloneTree(replacements, fromDir, toDir):
    """
    Clone a directory tree, making replacements into the
    file contents and the directory paths. Replacements
    is a list of string pairs.
    """
    with os.popen('cd {}; git ls-files'.format(fromDir)) as g:
        files = [path.strip() for path in g.readlines()]

    for f in files:
        srcFile =  fromDir / f
        targetFile = toDir / stringWithReplacements(replacements, f)
        os.makedirs(targetFile.parent, exist_ok=True)
        if srcFile.is_file():
          print ("writing", targetFile)
          with open(srcFile, 'rb') as fromfile:
              with open(targetFile, 'wb') as tofile:
                  context = fromfile.read()
                  tofile.write(bytesWithReplacements(replacements, context))
          os.chmod(targetFile, os.stat(srcFile).st_mode)
        elif srcFile.is_symlink():
            ltarget = os.readlink(srcFile)
            ltarget = stringWithReplacements(replacements,ltarget)
            os.symlink(ltarget, targetFile)

def stringWithReplacements(replacements, s):
    for (fromv, tov) in replacements:
        s = s.replace(fromv, tov)
    return s

def bytesWithReplacements(replacements, bs):
    for (fromv, tov) in replacements:
        bs = bs.replace(bytes(fromv,'utf-8'), bytes(tov,'utf-8'))
    return bs


def dockerCompose(composefile, *args):
    """
    Returns a doit action that runs docker compose on the specified compose file, with the
    specified arguments
    """
    def action():
        env = os.environ.copy()
        env['USERGROUPID'] = '{}:{}'.format(os.getuid(), os.getgid())
        subprocess.run(['docker-compose', '-f', composefile.name] + [str(arg) for arg in args], cwd=str(composefile.parent), check=True, env=env)
    return action
