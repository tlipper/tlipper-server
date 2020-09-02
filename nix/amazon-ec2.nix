let

  region = "eu-west-1";
  accessKeyId = "default"; # symbolic name looked up in ~/.ec2-keys or a ~/.aws/credentials profile name

  ec2 =
    { resources, ... }:
    { deployment.targetEnv = "ec2";
      deployment.ec2.accessKeyId = accessKeyId;
      deployment.ec2.region = region;
      deployment.ec2.ami = "ami-0fe40176548ff0940";
      deployment.ec2.ebsInitialRootDiskSize = 100;
      deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
    };

in
{ tlipperserver = ec2;

  # Provision an EC2 key pair.
  resources.ec2KeyPairs.my-key-pair =
    { inherit region accessKeyId; };
}
