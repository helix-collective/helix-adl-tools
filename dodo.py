from hx.dodo_helpers import *
from datetime import date
import getpass

HOME = Path(os.environ['HOME'])
HERE = Path('.')

hxadlhszip = HERE/'build/hxadl-hs.zip'
hxadltszip = HERE/'typescript/hx-adl/build/hxadl-ts.zip'

hxadlimagebuilt = MarkerFile(HERE/'build/.hxadlimagebuilt')
hxadlimagepushed = MarkerFile(HERE/'build/.hxadlimagepushed')

nodemodules = YarnNodeModules(HERE / 'typescript/hx-adl')

checkNodeVersion = checkNode(minVersion='8.9.1',maxVersion="11.0.0")

def task_node_modules():
    return nodemodules.task()

def task_build_haskell():
    filedeps = [f for f in rglobfiles(HERE/'haskell') if '.stack-work' not in str(f)]
    distdir = HERE/'build/dist'

    return {
        'doc' : 'build adlc and hx-adl for linux',
        'actions': [
            stack(['docker', 'pull']),
            stack(['build']),
            'mkdir -p {0}/bin'.format(distdir),
            'cp $({}) {}/bin'.format(stack(['exec', 'which', '--', 'adlc']),distdir),
            'cp $({}) {}/bin'.format(stack(['exec', 'which', '--', 'hx-adl-hs']),distdir),
            'cp -r $({})/.. {}/lib'.format(stack(['exec', 'adlc', '--', 'show', '--adlstdlib']), distdir),
            'cd {}; zip -q -r {} *'.format(distdir, hxadlhszip.resolve())
        ],
        'file_dep': filedeps,
        'targets': [hxadlhszip],
        'verbosity' : 2,
        'clean': True
    }

def task_genadl():
  return {
       'doc' : 'Regenerate the typescript runtime from the adl',
       'actions': [
          stack(['build']),
          stack(['exec','adlc','--','typescript',
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


def task_docker_build_hxadl_image():
    installsh = HERE/'platform/docker/install.sh'
    hxadlsh = HERE/'platform/docker/hx-adl.sh'
    packagejson = HERE/'typescript/hx-adl/package.json'
    yarnlock = HERE/'typescript/hx-adl/yarn.lock'

    context = DockerContext()
    context.file(hxadlhszip, hxadlhszip.name)
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
    image.cmd('COPY {} /tmp'.format(hxadlhszip.name))
    image.cmd('RUN unzip /tmp/{0} -d /opt && rm -r /tmp/{0}'.format(hxadlhszip.name))
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

def stack(args):
    return 'stack --stack-yaml={} --docker {}'.format(HERE/'haskell/stack.yaml',' '.join(args))

