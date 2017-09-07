
# First use "aws configure" in the Shell to link the proper credentials to the s3 buckets

aws s3 cp s3://earthlab-natem/data/anthro ../data/anthro  --recursive
aws s3 cp s3://earthlab-natem/data/fire ../data/fire --recursive
aws s3 cp s3://earthlab-natem/data/bounds ../data/bounds --recursive