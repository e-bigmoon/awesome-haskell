# INTRO TO DEVOPS ON GOVCLOUD

## WHAT I WOULD HAVE WANTED TO KNOW ABOUT AWS GOVCLOUD

While assisting a US municipal government with their cloud migration, we recently had the opportunity to deploy a complete hosting platform to the [GovCloud Region](https://aws.amazon.com/jp/govcloud-us/). Our task was to provide a platform based on kubernetes, running within a secure VPC built on private subnets and with VPN links to an enterprise-class network that spanned multiple datacenters.

While we had researched particulars of GovCloud before getting into the project, we ran into a few surprises along the way. This is the type of post I had wanted to find earlier.

This post aims to provide an overview of the AWS "GovCloud Region", how it is not just another region in the AWS cloud you have come to know and understand, the types of problems you will run into when you go to deploy your services to GovCloud, and notes on how we overcame these obstacles to use GovCloud effectively while reusing our existing code and experience for AWS. This also serves as an introductory post to a mini-series on “Deploying to GovCloud” - be sure to check back on the follow-up posts where we’ll get deeper into solutions for the problems discussed here.

Consider this a supplement to the AWS doc on specific [differences between GovCloud and the "standard regions"](http://docs.aws.amazon.com/govcloud-us/latest/UserGuide/govcloud-differences.html).

## WHAT IS GOVCLOUD?

When reading the [AWS documentation on GovCloud](http://docs.aws.amazon.com/govcloud-us/latest/UserGuide/welcome.html) (and both the [FAQ](https://aws.amazon.com/jp/govcloud-us/faqs/) and the [User's Guide](http://docs.aws.amazon.com/govcloud-us/latest/UserGuide/govcloud-us-ug.pdf)), you get the impression "it's just another region" (perhaps with a few minor changes). In practice, the difference with GovCloud regions is more fundamental than that.

GovCloud is a separate cloud "partition". At the moment there is one "region" in GovCloud, us-gov-west-1, though it's likely that additional regions will come online in the future (AWS has noted a US-East region coming in 2018). Interestingly, there are also other "partitions", for a total of 3: aws, aws-us-gov, and aws-cn (China). For details on those partitions, see the [mini section on partitions in this doc](http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html).

## WHY WOULD YOU WANT TO USE GOVCLOUD?

Amazon GovCloud is AWS's isolated cloud partition for government contractors and agencies needing to host Controlled Unclassified Information (CUI). According to Amazon itself, "AWS GovCloud (US) gives vetted government customers and their partners the flexibility to architect secure cloud solutions that comply with: the FedRAMP High baseline, the DOJ's Criminal Justice Information Systems (CJIS) Security Policy, U.S. International Traffic in Arms Regulations (ITAR), Department of Defense (DoD) Cloud Computing Security Requirements Guide (SRG) for Impact Levels 2, 4 and 5, FIPS 140-2, IRS-1075, and other compliance regimes." [See also this PDF](https://d0.awsstatic.com/whitepapers/compliance/AWS_DOD_CSM_Reference_Architecture.pdf).

In other words,

- it's physically and logically separated from the public AWS cloud everyone else is using;
- both physical access to the hardware running the cloud, as well as virtual access to the management console is through a vetted, investigative process;
- GovCloud is suitable for government related services, platforms offered by government agencies, etc
- GovCloud complies with a number of different security regulations and policies your organization may need to follow;
- as a developer/provider for a service, it may be in your contract to host on GovCloud;
- and as a developer of a commercial or open source tool, you may want to provide resources (AMIs, etc) to users running on GovCloud.

## RESTRICTIONS/LIMITATIONS ON GOVCLOUD, AND HOW TO WORK AROUND THEM

Access to GovCloud is limited, you must have a justified reason and the proper credentials to get access. Few devops projectseven have access. This means AMIs for your favorite Kubernetes deployment tool are not likely to be available. It also means that—since your favorite devops tools may not get any testing on GovCloud—developers may not know about bugs until you run into and report them. You can work around this by testing your tools as thoroughly as possible in planning stages of the projects, and by taking on extra support tasks like building AMIs typically provided by upstream developer.

Different [API Endpoints](http://docs.aws.amazon.com/govcloud-us/latest/UserGuide/using-govcloud-endpoints.html) - devops tools need to support these endpoints (based on the GovCloud region).

IAM in the AWS Web Console blocks access to the "Switch Role" functionality that is normally available. It seems possible to assume roles with AWS tools on the CLI (but we didn't investigate that one to be sure, we were limited on time and wanted a consistent auth workflow across both the AWS console as well as the cli utilities).

Must use aws-us-gov in AWS Resource Numbers (ARNs) instead of aws, [see this doc](http://docs.aws.amazon.com/govcloud-us/latest/UserGuide/using-govcloud-arns.html) (as well as the previous reference on AWS cloud partitions) - devops scripts, resource management code, and Terraform modules, etc, may have hard-coded in the aws, and you may need to update that or add support for flexibility with the ARN. [Here is an example from the AWS provider in Terraform](https://github.com/hashicorp/terraform/issues/5307). Review your devops tools and scripts.

While there are DNS servers in the VPC, there is no Route53 service (API). This breaks many devops tools that make the assumption Route53 is / will be available (kops w/ kubernetes, for example). To be fair, some tools (like kops) provide an alternative to Route53 for bootstrapping the cluster, though our testing found the features to be buggy and not yet production quality. We worked around the need for Route53 by deploying our own self-healing and automated DNS solutions. A future post will dive into the details of our Route53 replacements.

While there are "egress-only" gateways, they are IPV6-only, and the managed NAT gateway service is unavailable, so you either need to provide your own NAT gateway (on an EC2 instance), or route 0.0.0.0/0 over your VPN/etc links and thru your Corporate Network. We used both methods to address the need for private network topologies and NAT gateways. A future post will dive into the details of NAT and network routing in our VPC.

Here is the [list of available services](http://docs.aws.amazon.com/govcloud-us/latest/UserGuide/using-services.html).. not all services are available, and some services (like [Direct Connect](http://docs.aws.amazon.com/govcloud-us/latest/UserGuide/setting-up-direct-connect.html)) may differ from their counterparts on the regular cloud. Be sure to review the AWS docs for each service you plan to use in your project.

While the "GovCloud Region" may appear to be another option in AWS' collection of regions, there are restrictions that are specific to GovCloud. These restrictions are likely to impact the project you run on GovCloud, and we recommend getting up to speed on the details as early as possible. We also have a service dedicated to helping you address your GovCloud requirements which you can find [here](https://www.fpcomplete.com/pricing).

We hope this introductory post has been helpful, stay tuned for the next post in this series on GovCloud where we will dive deeper into options for IAM, DNS and NAT (to work-around what isn't available), and running modern devops on AWS GovCloud in general.

If you liked this blog post you might also be interested in these posts:

- [DevOps Best Practices - Immutability](https://www.fpcomplete.com/blog/2016/11/devops-best-practices-immutability)
- [DevOps Best Practices - Multifaceted Testing](https://www.fpcomplete.com/blog/2016/11/devops-best-practices-multifaceted-testing)
- [Manage Secets on AWS with Credstash and TerraForm](https://www.fpcomplete.com/blog/2017/08/credstash)
