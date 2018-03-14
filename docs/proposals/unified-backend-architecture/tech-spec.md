# Technical Specification for Unified Backend Architecture

## Author and Document Owner

[Denis Shevchenko](https://iohk.io/team/denis-shevchenko/)

## Status

DRAFT

## Epic

[CHW-35](https://iohk.myjetbrains.com/youtrack/issue/CHW-35)

## Purpose

The purpose of this document is to describe Unified Backend Architecture.

We propose to unify the different use cases under a single architecture, starting from the crucial observation
that both interfacing with an hardware wallet or provide a mobile client can be seen as a specialisation of the
general case of having a "keyless" wallet backend operating with the private keys and cryptography "hosted"
on the client side.

## Prerequisites

The reader should be familiar with following concepts:

1. Daedalus frontend,
2. wallet backend web API,
3. work with wallets,
4. transaction processing.

Also it is assumed that the reader knows basic terminology from [Cardano Docs](https://cardanodocs.com/),
for example, UTXO.

## Assumptions

N/A, see **Prerequisites**.

## Requirements

Unification of the wallet backend architecture should allow to:

1. work with different hardware wallets in the same manner,
2. create lightweight Daedalus mobile clients (iOS/Android),
3. create lightweight Daedalus desktop clients (Windows/macOS/Linux).

The main benefit of such a lightweight client is independence from the growing blockchain: the client does not
store blockchain locally and does not wait for a "Syncing blocks".

## Key Idea

As mentioned before, the key idea is to unify the different use cases under a single architecture, we mean
hardware wallets, mobile clients and lightweight desktop clients.

In the case of [Ledger](https://www.ledgerwallet.com/) (or any other hardware wallet) the privte keys and the
cryptography are hosted on the device, whereas in a mobile client they are held on a smartphone. And this schema
fits for "featherweight Daedalus desktops" that do not want to host the growing blockchain on the computer's
storage.

More specifically, a Daedalus mobile client will consist of an iOS or Android application which will communicate
with a cluster of nodes that will keep the global UTXO state for the blockchain and that will be capable
to reconstruct each mobile wallet state without hosting any user-specific data.

## Multi-Tenant Architecture

Mobile clients will communicate with a clusters of nodes, so the basic requirement is an ability of one node to work
with particular number of clients simultaneously. For example, if 1 cluster has to serve 10000 mobile clients and
this cluster consists of 100 nodes, each node has to be able to work with 100 clients simultaneously and reliably.
Thus, the longest procedure is a new payment (creating new transaction), and even if all these 100 clients will
sign their transactions and send it for publishing at the same time, the node must be able to handle these
transactions reliably and as fast as possible.

To achieve this goal the node must implement robust concurrent model. More specifically, all requests to the wallet
web API endpoints must be handled concurrently.

**TODO**: To think about the slowest endpoints, like `/api/v1/transactions`: is it possible to handle them
fully concurrently? I mean publishing to the network, storing in the `wallet-db`, etc.

## Scalability

Initially clusters of nodes will be hosted by IOHK (on AWS EC2). And we need a mechanism of auto-scaling of the
cluster: it must be able to grow proportionally to growing number of clients.

**TODO**: To think about auto-scale supervisor. Suppose we have 1 cluster that consists of 100 nodes and each of node
can reliably serve 100 clients. If supervisor sees that number of clients connected to this cluster is growing (and
becomes more than 10000), it launches additional nodes.

**TODO 2**: Launching additional node is relatively expensive action (in terms of time and memory). So it is better
if one single node will be able to serve more clients than just increase a number of nodes in the cluster.

## Workflow

These are typical workflows, with description of API calls.

### Initial State

After installation of mobile client application user does not see any wallets, and he must create at least one to
work with a client application.

During the first connection with the node the new **session** is establishing, please read an explanation below.

### Wallet Creation

User creates a new wallet. It produces a `POST`-request to `/api/v1/wallets` (with information about client's public
key), node creates a new wallet in `wallet-db` and sends response with information about new wallet.

Even if node `N` which serves the client `C` will die, another node `M` (the client `C` will reconnect to) will be able
to handle information about `C`'s wallet(s), because `wallet-db` is storing in the cluster.

### Wallet Import/Export

Wallet can be imported using client's secret key.

**TODO**: To clarify this process. User can be able to obtain its secret key.

### Wallet Restoring

Wallet can be restored. Use case: user changed a smartphone, install mobile Daedalus client and restore its wallet
using backup phrase (for example, 12 words) generated during this wallet creation.

Wallet restoring produces a `POST`-request to `/api/v1/wallets/?`, and node checks if such a wallet exists. If so,
response contains an information about the wallet.

**TODO**: To clarify this process.

### View Balance

User can see its current balance (by default, the balance should always be shown in the client application).

After client application launching and after transactions client is asking for current balance. It produces
`GET`(?)-request to `/api/v1/?`, and response contains an information about current balance on particular wallet.

Please note that user can have few wallets with different balances, so balance for current active wallet will be
asked and shown.

**TODO**: To clarify endpoints.

### Receive a Payment

User can receive a payment from other user. To do it he must provide a receive address.

**TODO**: To clarify details of receive address generation. Will it be generated on the client side on on the node
side? If latter - we need additional API endpoint, something like `/api/v1/address/receive/new`, to obtain new
receive address.

### Send a Payment

There are 3 steps to make a new payment:

1. create transaction,
2. sign transaction,
3. publish transaction.

When user creates a new payment (by defining recipient's address, amount and fee), it produces a `POST`-request
to `/api/v1/transactions/unsigned`, and response will contain a new transaction without a witness (signature).

After that user signs new transaction using its private keys (hosted on the client side).

Then user send a payment. It produces a `POST`-request to `/api/v1/transactions/signed`, node receives transaction
with a signature, forms transaction witness and publish this transaction to the Cardano network as usually.

## API Extension

API v1 must be extended to support both the mobile clients and the hardware wallets.

### Create Unsigned Transaction

When user wants to send money to other user, he creates a new payment. It produces a `POST`-request
to `/api/v1/transactions/unsigned` endpoint, and response will contain a new transaction without a
witness (signature).

### Send Signed Transaction

After user signs new transaction using its private keys (hosted on the client side) he sends it with
a signature. It produces a `POST`-request to `/api/v1/transactions/signed`, node receives transaction
with a signature, forms transaction witness and publish this transaction as usually.

## Wallet State

An analysis of the computational complexity of rebuilding the state of a wallet (e.g. its balance and
payments history) on the server in a way as much stateless as possible.

**TODO**: To think about it.

## Authentication Scheme

### HTTPS

Client must be sure that server is not a malicious one, so we use HTTPS-connection with a standard check
of TLS certs on the client side.

### Session

After successful TLS checking client and server establish a new **session**.

**TODO**: To think what is a session? What data is associated with the session? Does it have any timeout?

## Crypto Operations

A description of how the main operations on a crypto-wallet will be implemented, according to the final
index derivation mechanism chosen (sequential vs random).

**TODO**: To think about it.

## Security and Possible Attacks

### MITM

To prevent MITM attacks we have to include a fee in transaction explicitly.

Currently it is impossible, because transaction contains inputs, outputs and attributes, and we have to
know _previous_ transaction to calculate a fee. So we must change transaction's format for explicit
including a fee in it.

**TODO**: To think about it: should the fee be a part of transaction's attributes, or it should be a
separate field?

### Other Attacks

**TODO**: To think about it.
