module Domains.Site.Terms where

import Control.Monad.Reader.Class (class MonadAsk)
import Domains.Site.Markdown (useMarkdown)
import Effect.Class (class MonadEffect)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.Hooks as Hooks
import MarkdownIt (MarkdownIt)

terms = """
## Terms and Conditions

### Introduction

PureScript Domains is a free domain service (the "Service") owned and operated
by Nicholas Saunders (hereafter "we", "our", or "us"). The registration method
is to submit via
[pull request](https://github.com/purescript-domains/dns/compare) to the
_purescript-domains/dns_ GitHub repository a record (the "Record") that includes
a subdomain (the "Subdomain") selected according to the Name Policy described
herein. The pull request requires the submitter (hereafter "you" or "your") to
acknowledge and to accept these Terms and Conditions (hereafter the
"Agreement").

### Rights and responsibilities

You are responsible for maintaining the content referenced in the Record,
ensuring that it complies with the Content Policy described herein at all times.
We reserve the right to remove the Record if it references content that we
determine, in our sole discretion, violates this policy.

The Service is provided by us on a voluntary basis. You acquire no ownership or
other rights to the Service or to the Subdomain.

While we will strive to maintain the reliability of the Service, we necessarily
reserve the right to modify or to discontinue the Service at any time, for any
reason, without notice.

### Content Policy

Only content that is relevant to the PureScript community is permitted. Examples
of acceptable content include, but are not limited to, PureScript library
documentation and personal blogs that are focused (at least partially) on
PureScript-related topics.

Content that is illegal, obscene, indecent, or defamatory, incites racial or
ethnic hatred, or violates the rights of others is expressly forbidden.

A suspected content violation may be reported by
[filing an issue](https://github.com/purescript-domains/dns/issues/new) in the
_purescript-domains/dns_ repository.

### Name Policy

Subdomain names are generally offered on a first-come, first-served basis,
subject to the Content Policy and other terms of the Agreement.

The Subdomain name must match your GitHub username or the name of the
corresponding GitHub repository or organization, omitting the _purescript-_
prefix, if applicable.

In the case of a naming conflict, contact the current registrant. If you are
unable to resolve it, you may
[file an issue](https://github.com/purescript-domains/dns/issues/new) in the
_purescript-domains/dns_ repository. We will resolve the conflict after
considering the following factors:
* when the Subdomain name was registered
* whether the Subdomain complies with the Agreement
* the [PureScript registry](https://github.com/purescript/registry)
* commit history age and substance, if applicable
* number of GitHub stars

### Termination

You may terminate the Service at any time by removing the _`CNAME`_ file
referencing the Subdomain in your GitHub repository and submitting a
[pull request](https://github.com/purescript-domains/dns/compare) to remove the
Record.

### Limitation of liability

We provide the Service as-is, with no warranty of any kind and no
representations concerning the suitability, reliability, accuracy, completeness,
or timeliness of the Service.

We expressly disclaim all warranties, expressed or implied, in connection with
the Service, including conditions of merchantability, fitness for a particular
purpose, title, and non-infringement. We are not liable for direct, indirect,
punitive, special, or other damages, including without limitation, loss or delay
of use, lost profits, data loss, or any other damage in contract, tort, equity,
or any other legal theory, even if advised of the possibility thereof.

### Governing law and dispute resolution

Delivery and use of the service under this Agreement are subject to the
applicable laws of the State of Arizona.

You agree to and waive any objections to personal jurisdiction of and venue in
the state or federal courts of Maricopa County, Arizona.

### Changes to the Agreement

We reserve the right to alter the Agreement at any time, at our sole discretion,
without notice. You may return to this page periodically to review the current
version of the Agreement or monitor commit history here on GitHub.

Continued use of the Service after changes to the Agreement indicates that you
are bound by those changes. However, you can discontinue your use of the Service
as described below.

### Severability

If any term or provision of the Agreement is invalid, illegal, or unenforceable,
then the remainder of the Agreement will remain intact.
""" :: String

component
  :: forall r q i o m
   . MonadAsk { markdownIt :: MarkdownIt, markdownRef :: Ref Int | r } m
  => MonadEffect m
  => H.Component q i o m
component = Hooks.component \_ _ -> useMarkdown terms
