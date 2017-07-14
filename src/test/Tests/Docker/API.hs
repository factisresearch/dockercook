{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Docker.API (htf_thisModulesTests) where

import Cook.Docker.API
import Test.Framework

test_parseDockerTag :: IO ()
test_parseDockerTag =
    do assertEqual (DockerTag "ubuntu" (DockerTagVersionOther "14.04"))
                       (parseDockerTag "ubuntu:14.04")
       assertEqual (DockerTag "cook-5b520704fb2246ec2f688e708edba1e0ad7438" DockerTagVersionLatest)
                       (parseDockerTag "cook-5b520704fb2246ec2f688e708edba1e0ad7438")
       assertEqual (DockerTag "reg.foo.bar:5000/someimage" DockerTagVersionLatest)
                       (parseDockerTag "reg.foo.bar:5000/someimage")
       assertEqual (DockerTag "reg.foo.bar:5000/someimage" $ DockerTagVersionOther "vers")
                       (parseDockerTag "reg.foo.bar:5000/someimage:vers")
       assertEqual (DockerTag "<none>" DockerTagVersionNone)
                       (parseDockerTag "<none>:<none>")
