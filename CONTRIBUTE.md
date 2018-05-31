## Setup

This project use [stack](https://docs.haskellstack.org/en/stable/README/.
Dowload the project from github and run:
```
stack --stack-yaml=stack-8.?.yaml build
```
You can also build the doc with 
```
stack haddock
````

## faq

Why is there a makefile ? 
why is there no default stack.yml ? (required for some ide tools)

-> current master branch contains code for the **next release**, current release is on hackage and its source code is available in a branch