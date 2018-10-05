dockercook
=====

[![Build Status](https://travis-ci.org/factisresearch/dockercook.svg)](https://travis-ci.org/factisresearch/dockercook)

Build and manage multiple docker image layers to speed up deployment. For
a tutorial see below or the example directory.

# Install

Requirements: GHC7.8 and cabal or stack

* From Hackage: `cabal install dockercook`
* From Source: `git clone https://github.com/factisresearch/dockercook.git && cd dockercook && cabal install`
* From Source (stack): `git clone https://github.com/factisresearch/dockercook.git && cd dockercook && stack setup && stack install`

# Commands

```
Usage: dockercook [-v|--verbosity INT] COMMAND
  Speed up docker image building

Available options:
  -h,--help                Show this help text
  -v,--verbosity INT       log levels for 0 - 3

Available commands:
  cook                     Cook docker images
  check                    Validate a Dockercook file
  sync                     Sync local state with remote docker server
  version                  Show programs version
  timing                   Looking build times for image
  init                     Enable dockercook for current project / directory
```

# Cookfile Directives

## BASE COOK [cookfile]

Define the current cookfiles parent cookfile. You can only use this
directive once at the top and you can't use it in combination with `BASE
DOCKER [dockerimage]`.

## BASE DOCKER [dockerimage]

The cook-image will depend on an existing docker image. You can only use this
directive once at the top and you can't use it in combination with `BASE
DOCKER [dockerimage]`.

## INCLUDE [filepattern]

Include and depend on a file. You can use this anywhere in your cook file,
but it will be moved to the top before `UNPACK [target_dir]`.

## UNPACK [target_dir]

All included files will be unpacked to this directory. It will also ensure
that the directory exists. Requires a tar binary inside your
docker-container. You can only use this directive once.

## BEGIN

Begin a transaction. You can only put `SCRIPT [script]` and `RUN
[bash_cmd]` commands inside a transaction. All commands inside the
transaction will result in a single layer.

## COMMIT

Commit a transaction. This is only possible if you began a transaction ;-)

## SCRIPT [script]

Run a bash script and put it's result at the current position in the
dockerfile.

## DOWNLOAD [url] [filepath]

Download a file from `[url]` to `[filepath]` in your docker
container. The server must set one of the following headers and support
HEAD requests: Last-Modified, ETag, Content-MD5

## PREPARE [shell-command]

This shell command is executed in an empty directory and is useful to copy
additional files into the build context. Any file that you copy in the
working directory of the shell-command will be available in your cook file
from the `/_cookpreps` directory. All `PREPARE` commands in one file will be
executed in the same preparation directory.

After running all `PREPARE` commands, dockercook hashes all files
produced as a dependency checksum.  But sometimes, the content of a file
is non-deterministic although it's semantics is the same. In these
situations, taking the hash of the file unnecessarily produces different
images.  As a solution, you can generate a file called `.cookHash_FILE`
along with `FILE`.  If the `PREPARE` command finds such a file in the same
directory as `FILE`, it hashes the `.cookHash_FILE` instead of `FILE`.

For more information on `PREPARE` check the example.

## COOKCOPY [cookfile] [image-dir] [target-dir]

Copy a file or directory from an image described by `[cookfile]` to
`/_cookpreps/[target-dir]` in your current context. This can be very
useful for binary-only containers. Behind the scenes this starts the
image (entrypoint is set to `""`), uses `docker cp` to extract files and then kills the container.

## COOKVAR [var-name] [default-value]

This allows compile time environment variables. They can be set using `--set-var` (multiple times) with the `dockercook cook` command. The default value is optional. A `COOKVAR` directive is translated to dockers `ENV` command and populated with the correct value. If a `COOKVAR` does not have a default value and is not supplied via `--set-var` the build will fail.

## All Docker-Commands

Most other docker-commands are allowed in Cookfiles. `ADD` and `COPY`
commands are not recommended, as the dependencies aren't tracked. The
`FROM` command is not allowed.

# Emacs support

There's a basic `cookfile-mode.el` in the repository :-)

# Motivation / Tutorial

Consider the following Dockerfile for a sample nodejs project:

### Dockerfile
```
FROM ubuntu:14.04
RUN apt-get update
RUN apt-get install -y nodejs npm
RUN ln -s /usr/bin/nodejs /usr/bin/node
RUN mkdir /app
ADD package.json /app/package.json
WORKDIR /app
RUN npm install
ADD . /app
CMD node ./app.js 
```

We have two branches for this nodejs project with the following `package.json`:

### Branch A: package.json
```
{
    "name": "sample-app",
    "version": "0.1.0",
    "dependencies" : {
        "bloomfilter"   :  "0.0.12",
        "express" :  "2.1.x",
        "mongoose" :  "2.2.x",
        "moment": "2.5.x"
    }
}
```

### Branch B: package.json
```
{
    "name": "sample-app",
    "version": "0.1.1",
    "dependencies" : {
        "bloomfilter"   :  "0.0.12",
        "express" :  "3.4.x",
        "mongoose" :  "3.6.x",
        "moment": "2.5.x",
        "request": "2.34.x"
    }
}
```

Building these two branches alternately on the same machine using docker and the Dockerfile above you'll notice the following behaviour:

* docker build branch A (from scratch)
* docker build branch B (starts at “ADD package.json /app”)
* docker build branch B (from cache)
* docker build branch A (starts at “ADD package.json /app”)
* docker build branch A (from cache)
* docker build branch B (starts at “ADD package.json /app”)

Lot's of time is wasted reinstalling the packages over and over again. (See: [Build caching: what invalids cache and not?](http://kimh.github.io/blog/en/docker/gotchas-in-writing-dockerfile-en/#build_caching_what_invalids_cache_and_not))

You can solve this by [building more efficient Dockerfiles](http://bitjudo.com/blog/2014/03/13/building-efficient-dockerfiles-node-dot-js/), but then you'd need two "package images" and two "app images" for your two branches and manage, delete and update these manually.

`dockercook` solves this issue by slicing the repository and managing those "intermediate" images for you. Back to our sample node js app, you would create three `cook` Files:

### system.cook
```
BASE DOCKER ubuntu:14.04
BEGIN
RUN apt-get update
RUN apt-get install -y nodejs npm
RUN apt-get clean
RUN ln -s /usr/bin/nodejs /usr/bin/node
COMMIT
```

### node-pkg.cook
```
BASE COOK system.cook
INCLUDE package.json
UNPACK /app
WORKDIR /app
RUN npm install
```

### app.cook
```
BASE COOK node-pkg.cook
INCLUDE *.js
UNPACK /app
CMD node ./app.js
```

Now you'd build your repository branches using `dockercook cook`:

```
$ cd $HOME/my-repo
$ dockercook init # only the first time
$ git checkout branchA
...
$ dockercook cook cookfiles/app.cook
...
$ cd $HOME/my-repo && git checkout branchB
...
$ dockercook cook cookfiles/app.cook
```
You'll notice the following behaviour:

* build branch A (builds: system + node-pkg + app)
* build branch B (builds: node-pkg’ + app’)
* build branch B (builds: app’)
* build branch A (builds: app)
* build branch A (builds: -)
* build branch B (builds: -)

Lot's of time is saved because you don't need to reinstall all your packages dependencies everytime you switch branches.

# Related work

* [docker-buildcache](https://github.com/baremetal/docker-buildcache)
