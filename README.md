dockercook
=====

[![Build Status](https://travis-ci.org/factisresearch/dockercook.svg)](https://travis-ci.org/factisresearch/dockercook)

# Install

* From Source: `git clone https://github.com/factisresearch/dockercook.git && cd dockercook && cabal install`

# Commands

```
$ dockercook help
$ dockercook cook [args]
$ dockercook clean [args]
```

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
RUN apt-get update
RUN apt-get install -y nodejs npm
RUN ln -s /usr/bin/nodejs /usr/bin/node
```

### node-pkg.cook
```
BASE COOK system.cook
INCLUDE package.json
RUN mkdir /app
ADD package.json /app/package.json
WORKDIR /app
RUN npm install
```

### app.cook
```
BASE COOK node-pkg.cook
INCLUDE *.js
ADD . /app
CMD node ./app.js
```

Now you'd build your repository branches using `dockercook cook`:

```
$ cd $HOME/my-repo && git checkout branchA
...
$ dockercook cook --state /tmp/build-state --data $HOME/my-repo --buildfiles $HOME/my-repo/cookfiles --entrypoint app.cook
...
$ cd $HOME/my-repo && git checkout branchB
...
$ dockercook cook --state /tmp/build-state --data $HOME/my-repo --buildfiles $HOME/my-repo/cookfiles --entrypoint app.cook
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

