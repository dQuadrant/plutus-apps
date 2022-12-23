## Cardano-Rpc 



### 1. Updated cardano-cli 
https://github.com/dquadrant/cardano-node

- Adds `cardano-cli rpc `  subcommand

### 2. Updated marconi-mamba
https://github.com/dquadrant/plutus-apps
 
 - Fixes existing issues in marconi-mamba inmemory querying
 - Adds simpler implementation of json-rpc server.


### 3. Testing the current non-production version

-3.1 Start cardano-node and set the socket path environment variable.
```
   export CARDANO_NODE_SOCKET_PATH=/home/user/.cardano/preprod/node.socket
```

-3.2 Start marconi-mamba service
```
    git clone https://github.com/dquadrant/plutus-apps
    cd plutus-apps
    git checkout feature/rpc
    nix-shell
    cabal update
    cabal run exe:marconi-mamba  -- --socket-path $CARDANO_NODE_SOCKET_PATH --testnet-magic 1 --addresses-to-index '*' --utxo-db ./.data --http-port 3345  
```

-3.3 Try out cardano-cli rpc commands
```
    git clone https://github.com/dquadrant/cardano-node
    cd cardano-node
    git checkout feature/rpc
    nix-shell
    cabal update
    # getUtxos
    cabal run cardano-cli -- rpc ada_getUtxos addr_test1qrmntnd29t3kpnn8uf7d9asr3fzvw7lnah55h52yvaxnfe4g2v2ge520usmkn0zcl46gy38877hej5cnqe6s602xpkyqtpcsrj
    # getBalance
    cabal run cardano-cli -- rpc ada_getBalance addr_test1qrmntnd29t3kpnn8uf7d9asr3fzvw7lnah55h52yvaxnfe4g2v2ge520usmkn0zcl46gy38877hej5cnqe6s602xpkyqtpcsrj
```

### 4. Using marconi and marconi-mamba
 we found that marconi and marconi-mamba is not properly documented and had bugs we had to fix before using it. Marconi and marconi-mamba project needs further polishing and also refactoring for making it easily integratable with  backends other than sqlite.

### 5. Full implementation
 We found that json-rpc is versatile and add more features/methods to it is easy.
 Json-rpc can be used not just for querying data but can also be extended for event subscription.


Future implementation can add following rpcs, but it's not limited to it.
# query (including historical data)
- ada_getBlock (blockHash)
- ada_getTransaction (txid ,includemempool=true,false)
- ada_getTxout (txhash, index, includemempool=true,false)
- ada_getCode (scriptHash)
- ada_getData (datumHash)


# query current state
- ada_getTip
- ada_getProtocolParams
- ada_getEraHistory
- ada_getSystemStart
- ada_getUtxo (maybe spend credential, maybe stake credential, maybe dataHash, maybe scriptHash,minconfirmation=n)  where (0>n>=100) (n=0) means to  include mempoolTxToo
- ada_getBalance (maybe spend credential, maybe stake credential,minConfirmation = n) where (0>n>=100) (n=0) means to  include mempoolTxToo


## pools and delegation
- ada_getPools
- ada_getPool (id)
- ada_getDelegation (stakeAddr)
- ada_getReward (stakeAddr)
- ada_getDelegations (PoolId)


## events
- ada_subscribeDelegation (stakeAddr or poolId)
- ada_subscribeTransfer	(maybe spend credential,maybe stake credential)
- ada_subscribeBlock (start,end)
