# smack-scala
Akka based Scala wrapper for [Smack](https://github.com/igniterealtime/Smack) (XMPP/Jabber) with support for [XEP-0066 out of band file transfers](http://xmpp.org/extensions/xep-0066.html)

## Running tests requires a running ejabberd
* ensure ejabberd is running
* configure admin user, domain and host in src/test/resources/application.conf

## AWS requires credential file configuration
* Set up credentials for [AWS](http://docs.aws.amazon.com/AWSSdkDocsJava/latest/DeveloperGuide/credentials.html#credentials-file-format)
