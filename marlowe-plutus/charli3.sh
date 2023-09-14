#!/usr/bin/env bash


LATEST=$(
psql -t -h 192.168.0.12 mainnet << EOI
select row_to_json(result)
  from (
    select
        encode(txid, 'hex') as txid
      , txix
      , encode(datumhash, 'hex') as datumhash
      , encode(datumbytes, 'hex') as datumbytes
      from chain.txout
        where (txid, txix) in (
          select txoutid, txoutix
            from chain.assetout
            inner join chain.asset
              on assetout.assetid = asset.id
            where policyid = '\xadb6f28429c860672e877f15f505a39899c1cdef9c68b2081a172736'
              and name = '\x4f7261636c6546656564'
          order by slotno desc
          limit 1
        )
  ) result
;
EOI
)

UTXO=$(echo "$LATEST" | jq -r '.txid + "#" + (.txix|tostring)')
echo "UTXO = $UTXO"

DATUM=$(echo "$LATEST" | jq -r '.datumbytes')
echo "DATUM = $DATUM"

DATUM_HASH=$(echo "$LATEST" | jq -r '.datumhash')
echo "DATUM HASH = $DATUM_HASH"

PRICE=$(
echo "$LATEST" \
| jq -r '.datumbytes' \
| tr '[:lower:]' '[:upper:]' \
| basenc --decode --base16 \
| cbordump -j \
| jq '.[0][0][0][0]' \
)
echo "PRICE = $(echo "scale=6; 100000 / $PRICE" | bc | sed 's/^\./0&/') USD/ADA"
