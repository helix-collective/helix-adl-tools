from hx.dodo_helpers import *
from datetime import date
import getpass

HOME = Path(os.environ['HOME'])
HERE = Path('.')

hxadlzip = HERE/'build/hxadl.zip'
hxadlimagebuilt = MarkerFile(HERE/'build/.hxadlimagebuilt')
hxadlimagepushed = MarkerFile(HERE/'build/.hxadlimagepushed')

def dockerized_stack(args,workdir):
    user = getpass.getuser()
    uid,gid = os.getuid(),os.getgid()
    cmd =  "docker run -it --rm "
    cmd += "-v /home/{0}/.stack:/home/{0}/.stack ".format(user)
    cmd += "-v /home/{0}/.ssh:/home/{0}/.ssh ".format(user)
    cmd += "-v {0}:{0} ".format(HERE.resolve())
    cmd += "-v /etc/passwd:/etc/passwd "
    cmd += "-v /etc/group:/etc/group "
    cmd += "-v /tmp:/tmp "
    cmd += "--user {}:{} ".format(uid,gid)
    cmd += "-w {0} ".format(workdir.resolve())
    cmd += "helixta/helixdev:2018-04-17 "
    cmd += "stack "
    cmd += ' '.join(args)
    return cmd

def task_build():
    filedeps = rglobfiles(HERE/'src')
    filedeps += rglobfiles(HERE/'lib')
    filedeps += [HERE/'stack.yaml',HERE/'helix-adl-tools.cabal']
    distdir = HERE/'build/dist'

    return {
        'doc' : 'build adlc and hx-adl for linux',
        'actions': [
            dockerized_stack(['build', '--no-nix', '&& mkdir -p {0}/bin && cp `stack --no-nix exec which -- hx-adl` {0}/bin && cp `stack --no-nix exec which -- adlc` {0}/bin && cp -r `stack --no-nix exec adlc -- show --adlstdlib`/.. {0}/lib'.format(distdir)], workdir=HERE),
            'cd {}; zip -q -r {} *'.format(distdir, hxadlzip.resolve())
        ],
        'file_dep': filedeps,
        'targets': [hxadlzip],
        'verbosity' : 2,
        'clean': True
    }

def task_genadl():
    return {
        'doc' : 'Regenerate code from local adl',
        'actions': [
        ],
        'verbosity' : 2,
        'uptodate' : [False]
    }

def task_docker_build_hxadl_image():
    context = DockerContext()
    context.file(hxadlzip, hxadlzip.name)

    image = DockerImage( 'hxadl', context)
    image.cmd( 'FROM ubuntu:16.04' )
    image.cmd( 'MAINTAINER Helix Team <support@helixta.com.au>' )
    image.cmd( 'RUN apt-get update && apt-get install -y zip libgmp-dev' )
    image.cmd( 'COPY {} /tmp'.format(hxadlzip.name) )
    image.cmd( 'RUN unzip /tmp/{0} -d /opt && rm -r /tmp/{0}'.format(hxadlzip.name) )
    image.cmd( 'ENV PATH="/opt/bin:${PATH}"')

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
