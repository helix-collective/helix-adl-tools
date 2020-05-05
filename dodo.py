from hx.dodo_helpers import *
from datetime import date
import getpass
import sys
import shutil
import os

HOME = Path(os.environ['HOME'])
HERE = Path('.')

hxadlhslinuxzip = HERE/'build/hxadl-linux-hs.zip'            # linux binaries
hxadlhsplatformzip = HERE/'build/hxadl-platform-hs.zip'      # platform binaries
hxadltszip = HERE/'typescript/hx-adl/build/hxadl-ts.zip'
hxadlbindist = HERE/'dist/hxadl-bindist.zip'

hxadlimagebuilt = MarkerFile(HERE/'build/.hxadlimagebuilt')
hxadlimagepushed = MarkerFile(HERE/'build/.hxadlimagepushed')

nodemodules = YarnNodeModules(HERE / 'typescript/hx-adl')

checkNodeVersion = checkNode(minVersion='8.9.1',maxVersion="13")

def task_node_modules():
    return nodemodules.task()

def task_build_haskell():
    filedeps = [f for f in rglobfiles(HERE/'haskell') if '.stack-work' not in str(f)]
    distdir = HERE/'build/dist'

    def actions(stackcmd, zipfile):
       return [
            stackcmd(['build']),
            'mkdir -p {0}/bin'.format(distdir),
            'cp $({}) {}/bin'.format(stackcmd(['exec', 'which', '--', 'adlc']),distdir),
            'cp $({}) {}/bin'.format(stackcmd(['exec', 'which', '--', 'hx-adl-hs']),distdir),
            'cp -r $({})/.. {}/lib'.format(stackcmd(['exec', 'adlc', '--', 'show', '--adlstdlib']), distdir),
            'cd {}; zip -q -r {} *'.format(distdir, zipfile.resolve())
        ]

    yield {
        'doc' : 'build adlc and hx-adl for linux via docker',
        'name': 'docker',
        'actions': [stack_docker(['docker', 'pull'])] + actions(stack_docker,hxadlhslinuxzip),
        'file_dep': filedeps,
        'targets': [hxadlhslinuxzip],
        'verbosity' : 2,
        'clean': True
    }

    yield {
        'doc' : 'build adlc and hx-adl for platform',
        'name': 'platform',
        'actions': actions(stack_platform,hxadlhsplatformzip),
        'file_dep': filedeps,
        'targets': [hxadlhsplatformzip],
        'verbosity' : 2,
        'clean': True
    }

def task_genadl():
  return {
       'doc' : 'Regenerate the typescript runtime from the adl',
       'actions': [
          stack_docker(['build']),
          stack_docker(['exec','adlc','--','typescript',
                 '--include-rt', '--include-resolver', '--runtime-dir=runtime',
                 '-O', 'typescript/hx-adl/src/adl-gen/', '-I../adl',
                 'haskell/adl/adl/stdlib/sys/adlast.adl', 'haskell/adl/adl/stdlib/sys/types.adl'
                ])
       ]
  }

def task_build_typescript():
    hxadl = HERE/'typescript/hx-adl'
    filedeps = rglobfiles(hxadl/'src')
    filedeps += [hxadl/'yarn.lock', hxadl/'package.json', hxadl/'tsconfig.json']
    filedeps += nodemodules.file_dep()

    buildAction = ' && '.join( [
        'cd {}'.format(hxadl),
        'rm -rf build',
        'yarn build',
        'cp package.json yarn.lock build',
        'cd build',
        'zip -r {} *'.format(hxadltszip.name),
      ])

    return {
        'doc' : 'build the hxadl typescript, and pack it into a zip file',
        'actions': [
            runChecksAction([checkNodeVersion]),
            buildAction
        ],
        'file_dep': filedeps,
        'targets': [hxadltszip],
        'clean' : True,
        'verbosity' : 2
    }

def task_build_release():

    def mkhxadlbindist():
      with tempfile.TemporaryDirectory() as wdir:
        def cmd(cmd):
          subprocess.run(cmd, cwd=wdir, shell=True,check=True)
        cmd('unzip -q {}'.format(os.path.abspath(hxadlhsplatformzip)))
        cmd('mkdir -p lib/js; cd lib/js; unzip -q {}'.format(os.path.abspath(hxadltszip)))
        with open(wdir + '/bin/hx-adl', 'w') as f:
          f.write('''
#!/bin/bash
INSTALLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"
export NODE_PATH=$INSTALLDIR/lib/js
export HXADLHS=$INSTALLDIR/bin/hx-adl-hs
export ADLC=$INSTALLDIR/bin/adlc
if [ ! -d "$NODE_PATH/node_modules" ]; then
  (cd $NODE_PATH; yarn)
fi
node $NODE_PATH/main.js "$@"
  ''')
        cmd('chmod +x bin/hx-adl')
        bindist = os.path.abspath(hxadlbindist)
        cmd('mkdir -p {0}; rm -f {1}; zip -r {1} *'.format(os.path.dirname(bindist), bindist))

    return {
        'doc' : 'Build a platform specific release archive',
        'actions': [mkhxadlbindist],
        'file_dep': [
          hxadlhsplatformzip,
          hxadltszip,
        ],
        'targets': [hxadlbindist],
        'verbosity' : 2,
        'clean' : True
    }

def task_install_release():

    def install_release(asversion, force):
        platform = sys.platform
        if platform.startswith('linux'):
            cachedir=Path(os.environ["HOME"])/".cache"/"hxadl"
        elif platform.startswith('darwin'):
            cachedir=Path(os.environ["HOME"])/"Library"/"Caches"/"hxadl"
        else:
            raise Exception(f'Unimplemented platform {platform}')
        dest=cachedir/asversion

        if dest.exists():
            if force:
                shutil.rmtree(str(dest), ignore_errors=True)
            else:
                raise FileExistsError(f"Destination {str(dest)} exists already")

        dest.mkdir(parents=True, exist_ok=True)
        subprocess.run(f'unzip -q {os.path.abspath(hxadlbindist)}', cwd=str(dest), shell=True, check=True)

    return {
        'doc': "build release and install in local cache dir for use by adlenv",
        'params':[{
            'name':'asversion',
            'short':'v',
            'long': 'version',
            'type': str,
            'default': "dev",
            'help': 'Install as hxadl version string'},
            {
            'name':'force',
            'short':'f',
            'long': 'force',
            'type': bool,
            'default': False,
            'help': 'replace existing'}
        ],
        'task_dep': [
            'build_release'
        ],
        'verbosity': 2,
        'actions': [(install_release,)]
    }

def task_docker_build_hxadl_image():
    installsh = HERE/'platform/docker/install.sh'
    hxadlsh = HERE/'platform/docker/hx-adl.sh'
    packagejson = HERE/'typescript/hx-adl/package.json'
    yarnlock = HERE/'typescript/hx-adl/yarn.lock'

    context = DockerContext()
    context.file(hxadlhslinuxzip, hxadlhslinuxzip.name)
    context.file(hxadltszip, hxadltszip.name)
    context.file(packagejson, packagejson.name)
    context.file(yarnlock, yarnlock.name)
    context.file(installsh, installsh.name)
    context.file(hxadlsh, hxadlsh.name)

    image = DockerImage( 'hxadl', context)
    image.cmd('FROM ubuntu:18.04')
    image.cmd('MAINTAINER Helix Team <support@helixta.com.au>')
    image.cmd('COPY install.sh /tmp/install.sh')
    image.cmd('RUN sh -x /tmp/install.sh && rm /tmp/install.sh')

    # First install the yarn dependences, so they are cached
    image.cmd('COPY package.json yarn.lock /tmp/')
    image.cmd('RUN cd /tmp && yarn install && mkdir -p /opt/lib/js && mv /tmp/node_modules /opt/lib/js')

    # Then install the typescript code
    image.cmd('COPY {} /tmp'.format(hxadltszip.name))
    image.cmd('RUN unzip /tmp/{0} -d /opt/lib/js'.format(hxadltszip.name))
    image.cmd('COPY {} /opt/bin/hx-adl'.format(hxadlsh.name))

    # Then the haskell code
    image.cmd('COPY {} /tmp'.format(hxadlhslinuxzip.name))
    image.cmd('RUN unzip /tmp/{0} -d /opt && rm -r /tmp/{0}'.format(hxadlhslinuxzip.name))
    image.cmd('ENV PATH="/opt/bin:${PATH}"')

    # and cleanup
    image.cmd('RUN rm -r /tmp/package.json /tmp/yarn.lock /tmp/*.zip')

    return {
        'doc' : 'the hxadl docker image containing adl tooling',
        'actions': [
            image.action(),
            hxadlimagebuilt.action()
        ],
        'file_dep': context.file_dep(),
        'targets': [hxadlimagebuilt.path],
        'verbosity' : 2,
        'clean' : True
    }

def task_docker_push_hxadl_image():
    tag = get_releasename()
    return {
        'doc' : 'push the hxadl docker image containing adl tooling',
        'actions': [
            'docker tag hxadl:latest helixta/hxadl:{0}'.format(tag),
            'docker push helixta/hxadl:{0}'.format(tag),
        ],
        'file_dep': [hxadlimagebuilt.path],
        'targets': [hxadlimagepushed.path],
        'verbosity' : 2,
        'clean' : True
    }

def stack_docker(args):
    return 'stack --stack-yaml={} --docker {}'.format(HERE/'haskell/stack.yaml',' '.join(args))

def stack_platform(args):
    return 'stack --stack-yaml={} {}'.format(HERE/'haskell/stack.yaml',' '.join(args))

