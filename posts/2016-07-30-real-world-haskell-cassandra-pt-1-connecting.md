---
layout: post
title: Real World Haskell - Cassandra Part 1: Connecting to a Cassandra
description: "A reproducible diary of my failures (and eventual success) in connecting to Cassandra with Haskell"
category: 
tags: [Haskell]
---

I use cassandra at work and was wondering what a complete Haskell solution would look like. I go
to hackage and type in "cassandra" and [these results](http://hackage.haskell.org/packages/search?terms=cassandra) are returned:


```
    cassandra-thrift library
    thrift bindings to the cassandra database
    library Last uploaded on Apr 21, 2013

    cassandra-cql library
    Haskell client for Cassandra's CQL protocol
    bsd3, library Last uploaded on Jul 30, 2015

    cql-io library
    Cassandra CQL client.
    library, mpl Last uploaded on Jun 17

    hscassandra library
    cassandra database interface
    bsd3, library Last uploaded on Mar 17, 2011

    cql library
    Cassandra CQL binary protocol.
    library Last uploaded on Jun 17

    cassy library
    A high level driver for the Cassandra datastore
    bsd3, library Last uploaded on Sep 2, 2014

    Quelea library
    Programming with Eventual Consistency over Cassandra.
    bsd3, library Last uploaded on Dec 13

    Thrift library
    Haskell bindings for the Apache Thrift RPC system
    library Last uploaded on Apr 12, 2013
```

I don't particularly want the cassandra-thrift library bindings, so I'll try out the cassandra-cql library.

```
/tmp λ stack new test-cassandra-cql simple
Downloading template "simple" to create project "test-cassandra-cql" in test-cassandra-cql/ ...

The following parameters were needed by the template but not provided: author-email, author-name, category, copyright, github-username
You can provide them in /home/cody/.stack/config.yaml, like this:
templates:
  params:
    author-email: value
    author-name: value
    category: value
    copyright: value
    github-username: value
Or you can pass each one as parameters like this:
stack new test-cassandra-cql simple -p "author-email:value" -p "author-name:value" -p "category:value" -p "copyright:value" -p "github-username:value"

Looking for .cabal or package.yaml files to use to init the project.
Using cabal packages:
- test-cassandra-cql/test-cassandra-cql.cabal

Selecting the best among 8 snapshots...

* Matches lts-6.9

Selected resolver: lts-6.9
Initialising configuration using resolver: lts-6.9
Total number of user packages considered: 1
Writing configuration to file: test-cassandra-cql/stack.yaml
All done.
```

Modify the test-cassandra-cql.cabal file to look like this:

```
name:                test-cassandra-cql
version:             0.1.0.0
synopsis:            Simple project template from stack
description:         Please see README.md
homepage:            https://github.com/githubuser/test-cassandra-cql#readme
license:             BSD3
license-file:        LICENSE
author:              Author name here
maintainer:          example@example.com
copyright:           2016 Author name here
category:            Web
build-type:          Simple
cabal-version:       >=1.10

executable test-cassandra-cql
  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5,
                       cassandra-cql
```


[Stackage]() doesn't yet have cassandra-cql so it'll fail when we try to build. I'll run `stack solver --update-config` followed by `stack build` first. I also didn't yet have cabal-install installed so I'll add on `stack install cabal-install`:

```
/tmp/test-cassandra-cql λ stack install cabal-install && stack solver --update-config && stack build
Cabal-1.22.8.0: download
Cabal-1.22.8.0: configure
Cabal-1.22.8.0: build
Cabal-1.22.8.0: copy/register
cabal-install-1.22.9.0: download
cabal-install-1.22.9.0: configure
cabal-install-1.22.9.0: build
cabal-install-1.22.9.0: copy/register
Completed 2 action(s).
Copying from /home/cody/.stack/snapshots/x86_64-linux/lts-6.9/7.10.3/bin/cabal to /home/cody/.local/bin/cabal

Copied executables to /home/cody/.local/bin:
- cabal
Using configuration file: stack.yaml
Using cabal packages:
- test-cassandra-cql.cabal

Using resolver: lts-6.9
Using compiler: ghc-7.10.3
Asking cabal to calculate a build plan...
Trying with packages from lts-6.9 as hard constraints...
Successfully determined a build plan with 1 external dependencies.

The following changes will be made to stack.yaml:
* Resolver is lts-6.9
* Dependencies to be added
    extra-deps:
    - cassandra-cql-0.5.0.2

Updated stack.yaml
hslogger-1.2.10: using precompiled package
Decimal-0.4.2: using precompiled package
network-info-0.2.0.8: using precompiled package
resource-pool-0.2.3.2: using precompiled package
uuid-types-1.0.3: using precompiled package
uuid-1.3.12: using precompiled package
cassandra-cql-0.5.0.2: download
cassandra-cql-0.5.0.2: configure
cassandra-cql-0.5.0.2: build
cassandra-cql-0.5.0.2: copy/register
test-cassandra-cql-0.1.0.0: configure
Configuring test-cassandra-cql-0.1.0.0...
test-cassandra-cql-0.1.0.0: build
Preprocessing executable 'test-cassandra-cql' for
test-cassandra-cql-0.1.0.0...
[1 of 1] Compiling Main             ( src/Main.hs, .stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/test-cassandra-cql/test-cassandra-cql-tmp/Main.o )
Linking .stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/test-cassandra-cql/test-cassandra-cql ...
test-cassandra-cql-0.1.0.0: copy/register
Installing executable(s) in
/tmp/test-cassandra-cql/.stack-work/install/x86_64-linux/lts-6.9/7.10.3/bin
Completed 8 action(s).
/tmp/test-cassandra-cql λ 
```

Cool, now it's installed and we are ready to write some code. Let's be lazy and peruse the [github link](https://github.com/the-real-blackh/cassandra-cql) I found on [cassandra-cql's hackage page](http://hackage.haskell.org/package/cassandra-cql). At the github page I see something like this:

```
 	Database/Cassandra 	executeTrans now takes consistency as a parameter. 	7 months ago
	tests 	executeTrans now takes consistency as a parameter. 	7 months ago
	.gitignore 	Use Data.Pool for connection pooling. 	2 years ago
	.travis.yml 	Lower casing language name in travis script. 	a year ago
	LICENSE 	Checkpoint: It installs and more stuff works. Results not typed yet. 	3 years ago
	README.md 	Edits to readme. 	a year ago
	Setup.hs 	Add Setup.hs 	3 years ago
	cassandra-cql.cabal 	Fixed map serialization 	11 months ago
	changelog.md 	ver 0.5.0.2 change: fix incorrect upper bound on base package. 	a year ago
```

No examples directory, but tests directories usually have something useful. When I go there I see:

```
 	.gitignore 	Added script for creating keyspace. 	a year ago
	Main.hs 	Fixed map serialization 	11 months ago
	create_keyspace.cql 	Added script for creating keyspace. 	a year ago
	example-autocreate-keyspace.hs 	remove unused code 	a year ago
	example-trans.hs 	executeTrans now takes consistency as a parameter. 	7 months ago
	example.hs 	Fixing authentication for CQL binary protocol v2. 	a year ago
	test-decimal.hs 	Added PasswordAuthenticator and associated code. 	2 years ago
	test-double.hs 	Added PasswordAuthenticator and associated code. 	2 years ago
	test-float.hs 	Added PasswordAuthenticator and associated code. 	2 years ago
	test-inet.hs 	Added PasswordAuthenticator and associated code. 	2 years ago
	test-list.hs 	Added PasswordAuthenticator and associated code. 	2 years ago
	test-pool.hs 	export newPool' which allows overriding pool configuration defaults 	2 years ago
	test-set.hs 	Added PasswordAuthenticator and associated code. 	2 years ago
	test-timestamp.hs 	Added PasswordAuthenticator and associated code. 	2 years ago
	test-timeuuid.hs 	Added PasswordAuthenticator and associated code. 	2 years ago
	test-varint.hs 	Added PasswordAuthenticator and associated code. 	2 years ago
```

Ah, there's an example.hs and here are it's contents:

```haskell
{-# LANGUAGE OverloadedStrings, DataKinds #-}

import Database.Cassandra.CQL
import Control.Monad
import Control.Monad.CatchIO
import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import System.Random

dropSongs :: Query Schema () ()
dropSongs = "drop table songs"

createSongs :: Query Schema () ()
createSongs = "create table songs (id uuid PRIMARY KEY, title ascii, artist varchar, femaleSinger boolean, timesPlayed int, comment text)"

insertSong :: Query Write (UUID, ByteString, Text, Bool, Int, Maybe Text) ()
insertSong = "insert into songs (id, title, artist, femaleSinger, timesPlayed, comment) values (?, ?, ?, ?, ?, ?)"

getSongs :: Query Rows () (UUID, ByteString, Text, Bool, Int, Maybe Text)
getSongs = "select id, title, artist, femaleSinger, timesPlayed, comment from songs"

getOneSong :: Query Rows UUID (Text, Int)
getOneSong = "select artist, timesPlayed from songs where id=?"

ignoreDropFailure :: Cas () -> Cas ()
ignoreDropFailure code = code `catch` \exc -> case exc of
    ConfigError _ _ -> return ()  -- Ignore the error if the table doesn't exist
    Invalid _ _ -> return ()
    _               -> throw exc

main = do
    -- let auth = Just (PasswordAuthenticator "cassandra" "cassandra")
    let auth = Nothing
    {-
    Assuming a 'test' keyspace already exists. Here's some CQL to create it:
    CREATE KEYSPACE test WITH replication = { 'class' : 'SimpleStrategy', 'replication_factor' : '1' };
    -}
    pool <- newPool [("localhost", "9042")] "test" auth -- servers, keyspace, maybe auth
    runCas pool $ do
        ignoreDropFailure $ liftIO . print =<< executeSchema QUORUM dropSongs ()
        liftIO . print =<< executeSchema QUORUM createSongs ()

        u1 <- liftIO randomIO
        u2 <- liftIO randomIO
        u3 <- liftIO randomIO
        executeWrite QUORUM insertSong (u1, "La Grange", "ZZ Top", False, 2, Nothing)
        executeWrite QUORUM insertSong (u2, "Your Star", "Evanescence", True, 799, Nothing)
        executeWrite QUORUM insertSong (u3, "Angel of Death", "Slayer", False, 50, Just "Singer Tom Araya")

        songs <- executeRows QUORUM getSongs ()
        liftIO $ forM_ songs $ \(uuid, title, artist, female, played, mComment) -> do
            putStrLn ""
            putStrLn $ "id            : "++show uuid
            putStrLn $ "title         : "++C.unpack title
            putStrLn $ "artist        : "++T.unpack artist
            putStrLn $ "female singer : "++show female
            putStrLn $ "times played  : "++show played
            putStrLn $ "comment       : "++show mComment

        liftIO $ putStrLn ""
        liftIO . print =<< executeRow QUORUM getOneSong u2
```

Replace everying in Main.hs of our project we created with the above. Now we can try `stack build` again:

```
test-cassandra-cql-0.1.0.0: configure
Configuring test-cassandra-cql-0.1.0.0...
test-cassandra-cql-0.1.0.0: build
Preprocessing executable 'test-cassandra-cql' for
test-cassandra-cql-0.1.0.0...

/tmp/test-cassandra-cql/src/Main.hs:5:8:
    Could not find module ‘Control.Monad.CatchIO’
    It is a member of the hidden package ‘MonadCatchIO-transformers-0.3.1.3@Monad_Jr5N1jwqWOiDadnjgHIYSh’.
    Perhaps you need to add ‘MonadCatchIO-transformers’ to the build-depends in your .cabal file.
    Use -v to see a list of the files searched for.

/tmp/test-cassandra-cql/src/Main.hs:6:8:
    Could not find module ‘Control.Monad.Trans’
    It is a member of the hidden package ‘monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH’.
    Perhaps you need to add ‘monads-tf’ to the build-depends in your .cabal file.
    It is a member of the hidden package ‘mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8’.
    Perhaps you need to add ‘mtl’ to the build-depends in your .cabal file.
    Use -v to see a list of the files searched for.

/tmp/test-cassandra-cql/src/Main.hs:8:18:
    Could not find module ‘Data.ByteString.Char8’
    It is a member of the hidden package ‘bytestring-0.10.6.0@bytes_6VWy06pWzJq9evDvK2d4w6’.
    Perhaps you need to add ‘bytestring’ to the build-depends in your .cabal file.
    Use -v to see a list of the files searched for.

/tmp/test-cassandra-cql/src/Main.hs:10:18:
    Could not find module ‘Data.Text’
    It is a member of the hidden package ‘text-1.2.2.1@text_HmqVQnZSpjaC156ABqPhne’.
    Perhaps you need to add ‘text’ to the build-depends in your .cabal file.
    Use -v to see a list of the files searched for.

/tmp/test-cassandra-cql/src/Main.hs:11:8:
    Could not find module ‘Data.UUID’
    It is a member of the hidden package ‘uuid-1.3.12@uuid_BjRErtmELFC2d9F8IOzq95’.
    Perhaps you need to add ‘uuid’ to the build-depends in your .cabal file.
    Use -v to see a list of the files searched for.

/tmp/test-cassandra-cql/src/Main.hs:12:8:
    Could not find module ‘System.Random’
    It is a member of the hidden package ‘random-1.1@rando_9Kgekc9yEaLHLNUuw6paWL’.
    Perhaps you need to add ‘random’ to the build-depends in your .cabal file.
    Use -v to see a list of the files searched for.

--  While building package test-cassandra-cql-0.1.0.0 using:
      /home/cody/.stack/setup-exe-cache/x86_64-linux/setup-Simple-Cabal-1.22.5.0-ghc-7.10.3 --builddir=.stack-work/dist/x86_64-linux/Cabal-1.22.5.0 build exe:test-cassandra-cql --ghc-options " -ddump-hi -ddump-to-file"
    Process exited with code: ExitFailure 1
```

Uh oh, that looks kind of scary. All it's saying though is that we don't have those packages listed in the build-depends section of our cabal file which currently looks like:

```
name:                test-cassandra-cql
version:             0.1.0.0
synopsis:            Simple project template from stack
description:         Please see README.md
homepage:            https://github.com/githubuser/test-cassandra-cql#readme
license:             BSD3
license-file:        LICENSE
author:              Author name here
maintainer:          example@example.com
copyright:           2016 Author name here
category:            Web
build-type:          Simple
cabal-version:       >=1.10

executable test-cassandra-cql
  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5,
                       cassandra-cql,
```

Let's update it to include all of those dependencies:

```
name:                test-cassandra-cql
version:             0.1.0.0
synopsis:            Simple project template from stack
description:         Please see README.md
homepage:            https://github.com/githubuser/test-cassandra-cql#readme
license:             BSD3
license-file:        LICENSE
author:              Author name here
maintainer:          example@example.com
copyright:           2016 Author name here
category:            Web
build-type:          Simple
cabal-version:       >=1.10

executable test-cassandra-cql
  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5,
                       cassandra-cql,
                       MonadCatchIO-transformers,
                       monads-tf,
                       bytestring,
                       text,
                       uuid,
                       random
```

And then run `stack build` again:

```
/tmp/test-cassandra-cql λ stack build 
test-cassandra-cql-0.1.0.0: configure
Configuring test-cassandra-cql-0.1.0.0...
test-cassandra-cql-0.1.0.0: build
Preprocessing executable 'test-cassandra-cql' for
test-cassandra-cql-0.1.0.0...
[1 of 1] Compiling Main             ( src/Main.hs, .stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/test-cassandra-cql/test-cassandra-cql-tmp/Main.o )
Linking .stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/test-cassandra-cql/test-cassandra-cql ...
test-cassandra-cql-0.1.0.0: copy/register
Installing executable(s) in
/tmp/test-cassandra-cql/.stack-work/install/x86_64-linux/lts-6.9/7.10.3/bin
```

It worked! The real question is: Why was I using stack build when next we're just going to use stack ghci. Ah well, time to use `stack ghci` and try to run our main function:

```
/tmp/test-cassandra-cql λ stack ghci 
Using main module: 1. Package `test-cassandra-cql' component exe:test-cassandra-cql with main-is file: /tmp/test-cassandra-cql/src/Main.hs
Configuring GHCi with the following packages: test-cassandra-cql
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
Ok, modules loaded: none.
[1 of 1] Compiling Main             ( /tmp/test-cassandra-cql/src/Main.hs, interpreted )
Ok, modules loaded: Main.
*Main> main 
failed to create a session due to temporary error (will retry) : NoAvailableServers
failed to create a session due to permanent error (will rethrow) : user interrupt
Interrupted.
```

Uh oh, we forgot to actually make a cassandra server available. We can use docker to quickly do that. If you don't have docker installed check out [docker's installation page](https://docs.docker.com/engine/installation/). So open up another terminal and type:

```
docker run -p 9042:9042 -p 9160:9160 -it --name some-cassandra cassandra:latest
```


It should look something like this:


```
/tmp/test-cassandra-cql λ docker run -p 9042:9042 -p 9160:9160 -it --name some-cassandra cassandra:latest
Unable to find image 'cassandra:latest' locally
latest: Pulling from library/cassandra
Digest: sha256:201f7a0fd29490032435945ee4985c0aa69eff36b2882e9a6d4b436eb140b1cc
Status: Downloaded newer image for cassandra:latest
INFO  15:57:05 Configuration location: file:/etc/cassandra/cassandra.yaml

... snip (random cassandra logs) ...

INFO  15:57:18 Scheduling approximate time-check task with a precision of 10 milliseconds
INFO  15:57:18 Created default superuser role 'cassandra'
```

Now if we go back to the other terminal and run our main function let's see what happens:

```
*Main> main 
failed to create a session due to temporary error (will retry) : LocalProtocolError "unexpected version 4" "<startup>"
failed to create a session due to permanent error (will rethrow) : user interrupt
```

Uh oh, it's an unexpected version. Maybe cassandra-cql doesn't support latest version of cassandra? Let's check the [docs](http://hackage.haskell.org/package/cassandra-cql) and the [source repo](). On the repo's readme.md it says:

"Haskell client for Cassandra's CQL binary protocol v2"

I believe the latest cassandra uses cql version 3.3, so we'll need a little older cassandra. 

Let's check what tags are available on the [docker cassandra page](https://hub.docker.com/_/cassandra/)

```
    2.1.15, 2.1 (2.1/Dockerfile)
    2.2.7, 2.2, 2 (2.2/Dockerfile)
    3.0.8, 3.0 (3.0/Dockerfile)
    3.7, 3, latest (3.7/Dockerfile)
```

Hm, let's guess and try 3.0.8.

```
/tmp/test-cassandra-cql λ docker rm -f some-cassandra && docker run -p 9042:9042 -p 9160:9160 -it --name some-cassandra cassandra:3.0.8
some-cassandra
INFO  18:07:43 Configuration location: file:/etc/cassandra/cassandra.yaml
INFO  18:07:53 Starting listening for CQL clients on /0.0.0.0:9042 (unencrypted)...

... snip (random cassandra logs) ...

INFO  18:07:53 Not starting RPC server as requested. Use JMX (StorageService->startRPCServer()) or nodetool (enablethrift) to start it
INFO  18:07:55 Created default superuser role 'cassandra'
```

Then try running our main function again:

```
*Main> main 
failed to create a session due to temporary error (will retry) : LocalProtocolError "unexpected version 4" "<startup>"
```

I'm not sure how cql versions/cassandra correspond so I'm just going to try the oldest tag and see if it works.

```
/tmp/test-cassandra-cql λ docker rm -f some-cassandra && docker run -p 9042:9042 -p 9160:9160 -it --name some-cassandra cassandra:2.1
some-cassandra
Unable to find image 'cassandra:2.1' locally
2.1: Pulling from library/cassandra

357ea8c3d80b: Already exists 
25eb4fd61cd9: Already exists 
a21cf6fac262: Already exists 
... snip (you know the rest by now) ...
```


Trying to run main again:

```
*Main> main 
failed to create a session due to permanent error (will rethrow) : Invalid "Keyspace 'test' does not exist" "USE test"
failed to create a session due to permanent error (will rethrow) : user interrupt
Interrupted.
```

Awesome! We're almost there, just need to create the keyspace. Can we do that with cassandra-cql? I [searched "keyspace"](https://github.com/the-real-blackh/cassandra-cql/search?utf8=%E2%9C%93&q=keyspace) on the cassandra-cql github and one of the results was "tests/example-autocreate-keyspace.hs". It looks like the example we copy pasted except it auto creates the keyspace. So copy the below into your main.hs:

```haskell
{-# LANGUAGE OverloadedStrings, DataKinds #-}

import Database.Cassandra.CQL
import Control.Monad
import Control.Monad.CatchIO
import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import System.Random

dropSongs :: Query Schema () ()
dropSongs = "drop table songs"

createSongs :: Query Schema () ()
createSongs = "create table songs (id uuid PRIMARY KEY, title ascii, artist varchar, femaleSinger boolean, timesPlayed int, comment text)"

insertSong :: Query Write (UUID, ByteString, Text, Bool, Int, Maybe Text) ()
insertSong = "insert into songs (id, title, artist, femaleSinger, timesPlayed, comment) values (?, ?, ?, ?, ?, ?)"

getSongs :: Query Rows () (UUID, ByteString, Text, Bool, Int, Maybe Text)
getSongs = "select id, title, artist, femaleSinger, timesPlayed, comment from songs"

getOneSong :: Query Rows UUID (Text, Int)
getOneSong = "select artist, timesPlayed from songs where id=?"

ignoreDropFailure :: Cas () -> Cas ()
ignoreDropFailure code = code `catch` \exc -> case exc of
    ConfigError _ _ -> return ()  -- Ignore the error if the table doesn't exist
    Invalid _ _ -> return ()
    _               -> throw exc

main = do
    -- let auth = Just (PasswordAuthenticator "cassandra" "cassandra")
    let auth = Nothing

    -- this config will automatically run keyspace creation cql script during each connection initializationj
    -- suitable for a development purposes

    let ksCfg  = "CREATE KEYSPACE IF NOT EXISTS test1 WITH replication = { 'class' : 'SimpleStrategy', 'replication_factor' : '1' };"
    let poolCfg = (defaultConfig [("localhost", "9042")] "test1" auth){ piKeyspaceConfig = Just ksCfg}

    pool <- newPool' poolCfg -- servers, keyspace, maybe auth
    runCas pool $ do
        ignoreDropFailure $ liftIO . print =<< executeSchema QUORUM dropSongs ()
        liftIO . print =<< executeSchema QUORUM createSongs ()

        u1 <- liftIO randomIO
        u2 <- liftIO randomIO
        u3 <- liftIO randomIO
        executeWrite QUORUM insertSong (u1, "La Grange", "ZZ Top", False, 2, Nothing)
        executeWrite QUORUM insertSong (u2, "Your Star", "Evanescence", True, 799, Nothing)
        executeWrite QUORUM insertSong (u3, "Angel of Death", "Slayer", False, 50, Just "Singer Tom Araya")

        songs <- executeRows QUORUM getSongs ()
        liftIO $ forM_ songs $ \(uuid, title, artist, female, played, mComment) -> do
            putStrLn ""
            putStrLn $ "id            : "++show uuid
            putStrLn $ "title         : "++C.unpack title
            putStrLn $ "artist        : "++T.unpack artist
            putStrLn $ "female singer : "++show female
            putStrLn $ "times played  : "++show played
            putStrLn $ "comment       : "++show mComment

        liftIO $ putStrLn ""
        liftIO . print =<< executeRow QUORUM getOneSong u2
```

Then we can reload in `stack ghci` and re-run our main function:

```
Prelude> :r
[1 of 1] Compiling Main             ( /tmp/test-cassandra-cql/src/Main.hs, interpreted )

/tmp/test-cassandra-cql/src/Main.hs:43:73:
    parse error on input ‘KeyspaceConfig’
Failed, modules loaded: none.
Prelude>  1
```

I know from experience that at this point it's probably because the version on github I copied the example from and the version on hackage differ. Sure enough after checking I see the hackage version is at 0.5.0.2 and the github version is at 0.6. I'm just going to use the github version, luckily stack makes that pretty easy.

Here is our current stack.yaml:

```yaml
flags: {}
extra-package-dbs: []
packages:
- '.'
extra-deps:
- cassandra-cql-0.5.0.2
resolver: lts-6.9
```

I've been busy doing Go programming at work and can't recall the git syntax for Stack, so I'll refer to this [big faq page they have](https://docs.haskellstack.org/en/stable/faq/). In about 15 seconds I see "I need to use a package (or version of a package) that is not available on hackage, what should I do?" and then:

```
To install packages directly from a Git repository, use e.g.:

resolver: lts-2.10
packages:
- location:
    git: https://github.com/githubuser/reponame.git
    commit: somecommitID
```

So we can update our stack.yaml from above to:

```yaml
flags: {}
extra-package-dbs: []
packages:
- '.'
- location:
    git: https://github.com/the-real-blackh/cassandra-cql.git
    commit: a34c7448630a25400ad20d42c5faca248f6fe644
resolver: lts-6.9
```

Then we can restart stack ghci:


```
Prelude> :r
[1 of 1] Compiling Main             ( /tmp/test-cassandra-cql/src/Main.hs, interpreted )

/tmp/test-cassandra-cql/src/Main.hs:43:73:
    ‘piKeyspaceConfig’ is not a (visible) constructor field name
Failed, modules loaded: none.
Prelude> :q
Leaving GHCi.
/tmp/test-cassandra-cql λ stack ghci
cassandra-cql-0.5.0.2: unregistering
cassandra-cql-0.6: configure
cassandra-cql-0.6: build
cassandra-cql-0.6: copy/register
test-cassandra-cql-0.1.0.0: configure
test-cassandra-cql-0.1.0.0: build
test-cassandra-cql-0.1.0.0: copy/register
Completed 2 action(s).
Using main module: 1. Package `test-cassandra-cql' component exe:test-cassandra-cql with main-is file: /tmp/test-cassandra-cql/src/Main.hs
Configuring GHCi with the following packages: test-cassandra-cql, cassandra-cql
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:3:23: Warning:
    -XOverlappingInstances is deprecated: instead use per-instance pragmas OVERLAPPING/OVERLAPPABLE/OVERLAPS

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:169:8:
    Ambiguous module name ‘Control.Monad.Reader’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:170:8:
    Ambiguous module name ‘Control.Monad.State’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:171:18:
    Ambiguous module name ‘Control.Monad.RWS’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:172:18:
    Ambiguous module name ‘Control.Monad.Error’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:173:18:
    Ambiguous module name ‘Control.Monad.Writer’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH
Failed, modules loaded: none.

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:3:23: Warning:
    -XOverlappingInstances is deprecated: instead use per-instance pragmas OVERLAPPING/OVERLAPPABLE/OVERLAPS

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:169:8:
    Ambiguous module name ‘Control.Monad.Reader’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:170:8:
    Ambiguous module name ‘Control.Monad.State’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:171:18:
    Ambiguous module name ‘Control.Monad.RWS’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:172:18:
    Ambiguous module name ‘Control.Monad.Error’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:173:18:
    Ambiguous module name ‘Control.Monad.Writer’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/src/Main.hs:6:8:
    Ambiguous module name ‘Control.Monad.Trans’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH
Failed, modules loaded: none.

<no location info>:
    Could not find module ‘Database.Cassandra.CQL’
    It is a member of the hidden package ‘cassandra-cql-0.6@cassa_FMk92m2DzAC8Q6AfWWp6qA’.
```


Whoa, it partially worked but then barfed on an error I have both not seen before and don't know what it is. I have a hunch it's something to do with multiple but different versions of cassandra-cql's dependencies. So I'll try something quick and easy, the equivalent of "turn it on and off again":


```
/tmp/test-cassandra-cql λ rm -r .stack-work/
/tmp/test-cassandra-cql λ stack build 
cassandra-cql-0.6: configure
cassandra-cql-0.6: build
cassandra-cql-0.6: copy/register
test-cassandra-cql-0.1.0.0: configure
test-cassandra-cql-0.1.0.0: build
test-cassandra-cql-0.1.0.0: copy/register
Completed 2 action(s).
/tmp/test-cassandra-cql λ stack ghci 
Using main module: 1. Package `test-cassandra-cql' component exe:test-cassandra-cql with main-is file: /tmp/test-cassandra-cql/src/Main.hs
Configuring GHCi with the following packages: test-cassandra-cql, cassandra-cql
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:3:23: Warning:
    -XOverlappingInstances is deprecated: instead use per-instance pragmas OVERLAPPING/OVERLAPPABLE/OVERLAPS

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:169:8:
    Ambiguous module name ‘Control.Monad.Reader’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:170:8:
    Ambiguous module name ‘Control.Monad.State’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:171:18:
    Ambiguous module name ‘Control.Monad.RWS’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:172:18:
    Ambiguous module name ‘Control.Monad.Error’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:173:18:
    Ambiguous module name ‘Control.Monad.Writer’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH
Failed, modules loaded: none.

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:3:23: Warning:
    -XOverlappingInstances is deprecated: instead use per-instance pragmas OVERLAPPING/OVERLAPPABLE/OVERLAPS

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:169:8:
    Ambiguous module name ‘Control.Monad.Reader’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:170:8:
    Ambiguous module name ‘Control.Monad.State’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:171:18:
    Ambiguous module name ‘Control.Monad.RWS’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:172:18:
    Ambiguous module name ‘Control.Monad.Error’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:173:18:
    Ambiguous module name ‘Control.Monad.Writer’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH

/tmp/test-cassandra-cql/src/Main.hs:6:8:
    Ambiguous module name ‘Control.Monad.Trans’:
      it was found in multiple packages:
      mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8 monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH
Failed, modules loaded: none.

<no location info>:
    Could not find module ‘Database.Cassandra.CQL’
    It is a member of the hidden package ‘cassandra-cql-0.6@cassa_FMk92m2DzAC8Q6AfWWp6qA’.
Prelude> 
```


I somehow figured out that this issue is caused by these libraries being in our cabal file:

```
name:                test-cassandra-cql
version:             0.1.0.0
synopsis:            Simple project template from stack
description:         Please see README.md
homepage:            https://github.com/githubuser/test-cassandra-cql#readme
license:             BSD3
license-file:        LICENSE
author:              Author name here
maintainer:          example@example.com
copyright:           2016 Author name here
category:            Web
build-type:          Simple
cabal-version:       >=1.10

executable test-cassandra-cql
  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5,
                       cassandra-cql,
                       MonadCatchIO-transformers,
                       monads-tf,
                       bytestring,
                       text,
                       uuid,
                       random
```

Update it to:

```
name:                test-cassandra-cql
version:             0.1.0.0
synopsis:            Simple project template from stack
description:         Please see README.md
homepage:            https://github.com/githubuser/test-cassandra-cql#readme
license:             BSD3
license-file:        LICENSE
author:              Author name here
maintainer:          example@example.com
copyright:           2016 Author name here
category:            Web
build-type:          Simple
cabal-version:       >=1.10

executable test-cassandra-cql
  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5,
                       cassandra-cql,
                       MonadCatchIO-transformers,
                       monads-tf,
                       bytestring,
                       text,
                       uuid,
                       random
```

Then run `stack ghci` again and you'll see those errors went away (though we have a lot of warnings):

```
/tmp/test-cassandra-cql λ stack ghci
test-cassandra-cql-0.1.0.0: build

--  While building package test-cassandra-cql-0.1.0.0 using:
      /home/cody/.stack/setup-exe-cache/x86_64-linux/setup-Simple-Cabal-1.22.5.0-ghc-7.10.3 --builddir=.stack-work/dist/x86_64-linux/Cabal-1.22.5.0 build exe:test-cassandra-cql --ghc-options " -ddump-hi -ddump-to-file"
    Process exited with code: ExitFailure 1
    Logs have been written to: /tmp/test-cassandra-cql/.stack-work/logs/test-cassandra-cql-0.1.0.0.log

    Preprocessing executable 'test-cassandra-cql' for
    test-cassandra-cql-0.1.0.0...
    
    /tmp/test-cassandra-cql/src/Main.hs:5:8:
        Could not find module ‘Control.Monad.CatchIO’
        It is a member of the hidden package ‘MonadCatchIO-transformers-0.3.1.3@Monad_Jr5N1jwqWOiDadnjgHIYSh’.
        Perhaps you need to add ‘MonadCatchIO-transformers’ to the build-depends in your .cabal file.
        Use -v to see a list of the files searched for.
    
    /tmp/test-cassandra-cql/src/Main.hs:6:8:
        Could not find module ‘Control.Monad.Trans’
        It is a member of the hidden package ‘monads-tf-0.1.0.3@monad_5M6wZtP1TpiAkFGzni5tIH’.
        Perhaps you need to add ‘monads-tf’ to the build-depends in your .cabal file.
        It is a member of the hidden package ‘mtl-2.2.1@mtl_Aue4leSeVkpKLsfHIV51E8’.
        Perhaps you need to add ‘mtl’ to the build-depends in your .cabal file.
        Use -v to see a list of the files searched for.
    
    /tmp/test-cassandra-cql/src/Main.hs:8:18:
        Could not find module ‘Data.ByteString.Char8’
        It is a member of the hidden package ‘bytestring-0.10.6.0@bytes_6VWy06pWzJq9evDvK2d4w6’.
        Perhaps you need to add ‘bytestring’ to the build-depends in your .cabal file.
        Use -v to see a list of the files searched for.
    
    /tmp/test-cassandra-cql/src/Main.hs:10:18:
        Could not find module ‘Data.Text’
        It is a member of the hidden package ‘text-1.2.2.1@text_HmqVQnZSpjaC156ABqPhne’.
        Perhaps you need to add ‘text’ to the build-depends in your .cabal file.
        Use -v to see a list of the files searched for.
    
    /tmp/test-cassandra-cql/src/Main.hs:11:8:
        Could not find module ‘Data.UUID’
        It is a member of the hidden package ‘uuid-1.3.12@uuid_BjRErtmELFC2d9F8IOzq95’.
        Perhaps you need to add ‘uuid’ to the build-depends in your .cabal file.
        Use -v to see a list of the files searched for.
Warning: build failed, but optimistically launching GHCi anyway
Using main module: 1. Package `test-cassandra-cql' component exe:test-cassandra-cql with main-is file: /tmp/test-cassandra-cql/src/Main.hs
Configuring GHCi with the following packages: test-cassandra-cql, cassandra-cql
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:3:23: Warning:
    -XOverlappingInstances is deprecated: instead use per-instance pragmas OVERLAPPING/OVERLAPPABLE/OVERLAPS
[1 of 1] Compiling Database.Cassandra.CQL ( /tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs, interpreted )

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:172:1: Warning:
    Module ‘Control.Monad.Error’ is deprecated:
      Use Control.Monad.Except instead

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:303:29: Warning:
    In the use of type constructor or class ‘Control.Monad.Error.Error’
    (imported from Control.Monad.Error, but defined in transformers-0.4.2.0:Control.Monad.Trans.Error):
    Deprecated: "Use Control.Monad.Trans.Except instead"

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:303:77: Warning:
    In the use of type constructor or class ‘Control.Monad.Error.ErrorT’
    (imported from Control.Monad.Error, but defined in transformers-0.4.2.0:Control.Monad.Trans.Error):
    Deprecated: "Use Control.Monad.Trans.Except instead"

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:837:38: Warning:
    Unticked promoted constructor: ‘Schema’.
    Use ‘'Schema’ instead of ‘Schema’.

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:843:55: Warning:
    Unticked promoted constructor: ‘Rows’.
    Use ‘'Rows’ instead of ‘Rows’.

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:1788:22: Warning:
    Unticked promoted constructor: ‘Rows’.
    Use ‘'Rows’ instead of ‘Rows’.

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:1799:23: Warning:
    Unticked promoted constructor: ‘Write’.
    Use ‘'Write’ instead of ‘Write’.

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:1816:21: Warning:
    Unticked promoted constructor: ‘Rows’.
    Use ‘'Rows’ instead of ‘Rows’.

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:1823:59: Warning:
    Unticked promoted constructor: ‘Rows’.
    Use ‘'Rows’ instead of ‘Rows’.

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:1835:23: Warning:
    Unticked promoted constructor: ‘Write’.
    Use ‘'Write’ instead of ‘Write’.

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:1847:24: Warning:
    Unticked promoted constructor: ‘Schema’.
    Use ‘'Schema’ instead of ‘Schema’.

/tmp/test-cassandra-cql/.stack-work/downloaded/MXPl4BU8uauT/Database/Cassandra/CQL.hs:1859:24: Warning:
    Unticked promoted constructor: ‘Schema’.
    Use ‘'Schema’ instead of ‘Schema’.
Ok, modules loaded: Database.Cassandra.CQL.
[2 of 2] Compiling Main             ( /tmp/test-cassandra-cql/src/Main.hs, interpreted )

/tmp/test-cassandra-cql/src/Main.hs:14:20: Warning:
    Unticked promoted constructor: ‘Schema’.
    Use ‘'Schema’ instead of ‘Schema’.

/tmp/test-cassandra-cql/src/Main.hs:17:22: Warning:
    Unticked promoted constructor: ‘Schema’.
    Use ‘'Schema’ instead of ‘Schema’.

/tmp/test-cassandra-cql/src/Main.hs:20:21: Warning:
    Unticked promoted constructor: ‘Write’.
    Use ‘'Write’ instead of ‘Write’.

/tmp/test-cassandra-cql/src/Main.hs:23:19: Warning:
    Unticked promoted constructor: ‘Rows’.
    Use ‘'Rows’ instead of ‘Rows’.

/tmp/test-cassandra-cql/src/Main.hs:26:21: Warning:
    Unticked promoted constructor: ‘Rows’.
    Use ‘'Rows’ instead of ‘Rows’.
Ok, modules loaded: Database.Cassandra.CQL, Main.
*Main Database.Cassandra.CQL> 
```

Now we can try to run our main function again:

```
*Main Database.Cassandra.CQL> main
(CREATED,Keyspace "TABLE",Table "test1")

id            : 8a33cc5c-5d1c-464f-ab3e-19c491f240d8
title         : Your Star
artist        : Evanescence
female singer : True
times played  : 799
comment       : Nothing

id            : 662f407a-7362-47c8-bc37-b859847a031a
title         : La Grange
artist        : ZZ Top
female singer : False
times played  : 2
comment       : Nothing

id            : 017e4971-88a2-48f1-b7c3-98cde924a6cc
title         : Angel of Death
artist        : Slayer
female singer : False
times played  : 50
comment       : Just "Singer Tom Araya"

Just ("Evanescence",799)
```

And success!

Part 2 will be a retrospective looking back at this process and how connecting to Cassandra in Haskell could be made easier for someone who might not have the luxury of time and patience that I had to get this working.
