import Principal "mo:base/Principal";
import Array "mo:base/Array";
import Int "mo:base/Int";
import List "mo:base/List";
import Nat "mo:base/Nat";
import Blob "mo:base/Blob";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Error "mo:base/Error";
import Result "mo:base/Result";
import Debug "mo:base/Debug";
import Iter "mo:base/Iter";
import Time "mo:base/Time";
import SHA224 "./SHA224";
import Hash "mo:base/Hash";

import Token "canister:token";

 actor Bridge {
    
    private stable var isInit = false;
    private stable var owner : Principal = Principal.fromText("aaaaa-aa"); 
    private stable var bot_messenger : Principal = Principal.fromText("aaaaa-aa"); 
    private stable var txcounter : Nat = 0;

    private stable var requestBridgingToEndInfoList = List.nil<RequestBridgingToEndInfo>();

    // type RequestBridgingToEndInfo = {
    //     caller : Principal;
    //     address : Text;
    //     timestamp : Int;
    //     amount : Nat;
    //     txHash : Text;
    // };
    type RequestBridgingToEndInfo = {
        id : Nat;
        caller : Principal;
        address : Text;
        amount : Nat;
    };
    public type TxReceipt = {
        #Ok : Nat;
        #Err : {
            #InsufficientAllowance;
            #InsufficientBalance;
            #ErrorOperationStyle;
            #Unauthorized;
            #LedgerTrap;
            #ErrorTo;
            #Other: Text;
            #BlockUsed;
            #AmountTooSmall;
        };
    };

    public func init(_owner : Principal, _bot_messenger : Principal) {
        assert(isInit == false);
        owner := _owner;
        bot_messenger := _bot_messenger;
        isInit := true;
    };

    public shared(msg) func requestBridgingToEnd(_amount : Nat, _address : Text) : async TxReceipt {
        let txReceipt : TxReceipt = await Token.transferFrom(msg.caller, Principal.fromActor(Bridge), _amount);
        switch (txReceipt){
            case (#Ok(blockIndex)) {
                //TODO: доработать с хэшем
                // let _timestamp : Int = Time.now();
                // let hashSum = getHashTx(msg.caller, _address, _timestamp, _amount);
                // let transactionInfo : RequestBridgingToEndInfo = {
                //     caller = msg.caller;
                //     address = _address;
                //     timestamp = _timestamp;
                //     amount = _amount;
                //     txHash = hashSum;
                // };
                // requestBridgingToEndInfoList := List.push(transactionInfo, requestBridgingToEndInfoList);
                // txcounter += 1;

                let transactionInfo : RequestBridgingToEndInfo = {
                    id = txcounter;
                    caller = msg.caller;
                    address = _address;
                    amount = _amount;
                };
                requestBridgingToEndInfoList := List.push(transactionInfo, requestBridgingToEndInfoList);
                txcounter += 1;
                return #Ok(blockIndex);
            };
            case (#Err(#InsufficientBalance)) {
                return txReceipt;
            };
            case (_) {
                return #Err(#ErrorTo);
            }
        };
        return #Err(#ErrorTo);
    };
   
    // public func getHashTx(_account : Principal, _address : Text, _timestamp : Int, _amount : Nat,) : async [Nat8] {
    //     let hash = SHA224.Digest();
    //     hash.write([0x0A]);
    //     hash.write(Blob.toArray(Principal.toBlob(_account)));
    //     hash.write(Blob.toArray(Text.encodeUtf8(_address)));
    //     hash.write(Blob.toArray(Text.encodeUtf8(Int.toText(_timestamp))));
    //     hash.write(Blob.toArray(Text.encodeUtf8(Nat.toText(_amount))));
    //     return hash.sum();
    // };

    public shared(msg) func performBridgingToStart(
        _amount : Nat,
        _account : Principal
        ) : async TxReceipt {
        if (msg.caller != owner) { 
            if (msg.caller != bot_messenger) { 
                return #Err(#Unauthorized);
            };
        };
        return await Token.transfer(_account, _amount);
    };

    public shared(msg) func canisterTokenBalance() : async Nat {
        await Token.balanceOf(Principal.fromActor(Bridge));
    };

    public shared(msg) func evacuateTokens(
        _principalTo : Principal, 
        _amount : Nat
        ) : async TxReceipt {
        if (msg.caller != owner) { 
            return #Err(#Unauthorized);
        } else {
           return await Token.transferFrom(Principal.fromActor(Bridge), _principalTo, _amount);
        };
    };

    public shared(msg) func setOwner(_owner : Principal) : async Bool {
        if (msg.caller != owner) { 
            return false;
        } else {
            owner := _owner;
            return true;
        };
    };

    public shared(msg) func setBotMassenger(_bot_messenger : Principal) : async Bool { 
        if (msg.caller != owner) { 
            return false;
        } else {
            bot_messenger := _bot_messenger;
            return true;
        };
    };

    public query func getOwner() : async Principal {
        return owner;
    };

    public query func getBotMassenger() : async Principal {
        return bot_messenger;
    };

    public query func getRequestBridgingToEndList() : async [RequestBridgingToEndInfo] {
        return List.toArray(requestBridgingToEndInfoList);
    };

    // public query func getRequestBridgingToEndListInterval(start : Nat) : async [RequestBridgingToEndInfo] {
    //     return List.toArray(requestBridgingToEndList);
    // };

    // public shared(msg) func deleteRequestBridgingToEndInfosFromList(
    //     _txHashs : [Text]
    //     ) : async Bool {
    //     var list = List.nil<RequestBridgingToEndInfo>();

    //     List.iterate<RequestBridgingToEndInfo>(requestBridgingToEndInfoList, func(obj : RequestBridgingToEndInfo) {
    //         for (_txHash : Text in Iter.fromArray(_txHashs)) {
    //             if (Text.equal(obj.txHash, _txHash) == false) {
    //                 list := List.push<RequestBridgingToEndInfo>(obj, list);
    //             };
    //         }; 
    //     });

    //     requestBridgingToEndInfoList := List.nil<RequestBridgingToEndInfo>();
    //     requestBridgingToEndInfoList := list;
            
    //     // for (RequestBridgingToEndInfo in List.iterate(_requestBridgingToEndInfoList)) {
    //     //     List.List.find(requestBridgingToEndInfoList, RequestBridgingToEndInfo);
    //     // };
    //     return true;
    // };

    public shared(msg) func deleteRequestBridgingToEndInfosFromList(
        _ids : [Nat]
        ) : async Bool {
        if (msg.caller != owner) { 
            if (msg.caller != bot_messenger) { 
                return false;
            };
        };
        // List.iterate<RequestBridgingToEndInfo>(requestBridgingToEndInfoList, func(obj : RequestBridgingToEndInfo) {
        //     for (_id in Iter.fromArray(_ids)) {
        //         Debug.print("1  " # debug_show _id);
        //         if (_id != obj.id) {
        //             Debug.print("2  " # debug_show _id);
        //             list := List.push<RequestBridgingToEndInfo>(obj, list);
        //         };
        //     }; 
        // });
        for (id in _ids.vals()) {
            requestBridgingToEndInfoList := await searchingForSingleItemInList(id);
        };

        // requestBridgingToEndInfoList := 
        // List.filter<RequestBridgingToEndInfo>(requestBridgingToEndInfoList,
        // func (obj : RequestBridgingToEndInfo) : Bool {
        //     for (_requestBridgingToEndInfo in List.toArray(list).vals()) {
        //         if (_requestBridgingToEndInfo == obj) {
        //             return false;
        //         } else return true;
        //     };
        //     return false;
        // });
            
        // for (RequestBridgingToEndInfo in List.iterate(_requestBridgingToEndInfoList)) {
        //     List.List.find(requestBridgingToEndInfoList, RequestBridgingToEndInfo);
        // };
        return true;
    };

    private func searchingForSingleItemInList(_id : Nat) : async List.List<RequestBridgingToEndInfo> {
        var list = List.nil<RequestBridgingToEndInfo>();
        for (_requestBridgingToEndInfo in List.toArray(requestBridgingToEndInfoList).vals()) {
            if (_requestBridgingToEndInfo.id != _id) {
                list := List.push<RequestBridgingToEndInfo>(_requestBridgingToEndInfo, list);
            };
        };
        return list;
    };

};
