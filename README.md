Odoric
======================================

## Dependencies

* getopt -- [https://github.com/jcomellas/getopt](https://github.com/jcomellas/getopt)
* erlcloud -- [https://github.com/gleber/erlcloud](https://github.com/gleber/erlcloud)

## Usage

To use S3 for uploading target systems you must have the environment
variable AWS_CREDENTIAL_FILE set to the location of your aws
credentials file. Example:

```shell
$ export AWS_CREDENTIAL_FILE=~/.aws
$ cat ~/.aws
AWSAccessKeyId=<YOUR ACCESS KEY>
AWSSecretKey=<YOUR SECRET KEY>
```

Now to create a target system and upload it to S3 we do:

```shell
$ sinan escript

$ ./_build/odoric/escript/odoric  deploy -u git@github.com:Mashape/odoric.git
```
