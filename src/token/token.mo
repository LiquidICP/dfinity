
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Types "./types";
import Time "mo:base/Time";
import Int "mo:base/Int";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Option "mo:base/Option";
import Order "mo:base/Order";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Debug "mo:base/Debug";
import ExperimentalCycles "mo:base/ExperimentalCycles";
import Cap "./cap/Cap";
import Root "./cap/Root";
import TypesCap "./cap/Types";

import Account "./Account";

import Ledger "canister:ledger";

actor Token {
    
    type Operation = Types.Operation;
    type TransactionStatus = Types.TransactionStatus;
    type TxRecord = Types.TxRecord;
    type Metadata = {
        logo_ : Text;
        name_ : Text;
        symbol_ : Text;
        decimals_ : Nat8;
        totalSupply_ : Nat;
        owner_ : Principal;
        fee_ : Nat;
    };
    // returns tx index or error msg
    public type TxReceipt = {
        #Ok: Nat;
        #Err: {
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
    public shared(msg) func init(
        _logo: Text,
        _name: Text, 
        _symbol: Text,
        _decimals: Nat8, 
        _owner: Principal,
        _feeWallet: Principal,
        _bot_messenger: Principal,
        _feeRate: Nat,
        _fee: Nat,
        ) {
            assert(isInit == false);
            logo := _logo;
            owner := _owner;
            bot_messenger := _bot_messenger;
            feeWallet := _feeWallet;
            name := _name;
            decimals := _decimals;
            symbol  := _symbol;
            fee := _fee;
            feeRate := _feeRate;
            feeTo := _owner;
            isInit := true;
        };

    private stable var owner : Principal = Principal.fromText("aaaaa-aa");
    private stable var bot_messenger : Principal = Principal.fromText("aaaaa-aa");
    private stable var feeWallet : Principal = Principal.fromText("aaaaa-aa");
    private stable var logo : Text = "";
    private stable var name : Text = "";
    private stable var decimals : Nat8 = 0;
    private stable var symbol : Text = "";
    private stable var totalSupply : Nat = 0;
    private stable var blackhole : Principal = Principal.fromText("aaaaa-aa");
    private stable var feeRate : Nat = 0;
    private stable var fee : Nat = 0; // transacrion fees
    private stable var isInit = false;
    private stable let MAX_BP = 1000;

    private stable var feeTo : Principal = Principal.fromText("aaaaa-aa");
    private stable var balanceEntries : [(Principal, Nat)] = [];
    private stable var allowanceEntries : [(Principal, [(Principal, Nat)])] = [];
    private var balances = HashMap.HashMap<Principal, Nat>(1, Principal.equal, Principal.hash);
    private var allowances = HashMap.HashMap<Principal, HashMap.HashMap<Principal, Nat>>(1, Principal.equal, Principal.hash);
    balances.put(owner, totalSupply);
    private stable let genesis : TxRecord = {
        caller = ?owner;
        op = #mint;
        index = 0;
        from = blackhole;
        to = owner;
        amount = totalSupply;
        fee = 0;
        timestamp = Time.now();
        status = #succeeded;
    };
    
    private stable var txcounter: Nat = 0;
    private var cap: ?Cap.Cap = null;
    private func addRecord(
        caller: Principal,
        op: Text, 
        details: [(Text, Root.DetailValue)]
        ): async () {
        let c = switch(cap) {
            case(?c) { c };
            case(_) { Cap.Cap(Principal.fromActor(Token), 2_000_000_000_000) };
        };
        cap := ?c;
        let record: Root.IndefiniteEvent = {
            operation = op;
            details = details;
            caller = caller;
        };
        // don't wait for result, faster
        ignore c.insert(record);
    };

    private func _chargeFee(from: Principal, fee: Nat) {
        if(fee > 0) {
            _transfer(from, feeTo, fee);
        };
    };

    private func _transfer(from: Principal, to: Principal, value: Nat) {
        let from_balance = _balanceOf(from);
        let from_balance_new : Nat = from_balance - value;
        if (from_balance_new != 0) { balances.put(from, from_balance_new); }
        else { balances.delete(from); };

        let to_balance = _balanceOf(to);
        let to_balance_new : Nat = to_balance + value;
        if (to_balance_new != 0) { balances.put(to, to_balance_new); };
    };

    private func _balanceOf(who: Principal) : Nat {
        switch (balances.get(who)) {
            case (?balance) { return balance; };
            case (_) { return 0; };
        }
    };

    private func _allowance(owner: Principal, spender: Principal) : Nat {
        switch(allowances.get(owner)) {
            case (?allowance_owner) {
                switch(allowance_owner.get(spender)) {
                    case (?allowance) { return allowance; };
                    case (_) { return 0; };
                }
            };
            case (_) { return 0; };
        }
    };

    private func u64(i: Nat): Nat64 {
        Nat64.fromNat(i)
    };

    /*
    *   Core interfaces:
    *       update calls:
    *           transfer/transferFrom/approve
    *       query calls:
    *           logo/name/symbol/decimal/totalSupply/balanceOf/allowance/getMetadata
    *           historySize/getTransaction/getTransactions
    */

    /// Transfers value amount of tokens to Principal to.
    public shared(msg) func transfer(to: Principal, value: Nat) : async TxReceipt {
        if (_balanceOf(msg.caller) < value + fee) { return #Err(#InsufficientBalance); };
        _chargeFee(msg.caller, fee);
        _transfer(msg.caller, to, value);
        ignore addRecord(
            msg.caller, "transfer",
            [
                ("to", #Principal(to)),
                ("value", #U64(u64(value))),
                ("fee", #U64(u64(fee)))
            ]
        );
        txcounter += 1;
        return #Ok(txcounter - 1);
    };

    /// Transfers value amount of tokens from Principal from to Principal to.
    public shared(msg) func transferFrom(from: Principal, to: Principal, value: Nat) : async TxReceipt {
        if (_balanceOf(from) < value + fee) { return #Err(#InsufficientBalance); };
        let allowed : Nat = _allowance(from, msg.caller);
        if (allowed < value + fee) { return #Err(#InsufficientAllowance); };
        _chargeFee(from, fee);
        _transfer(from, to, value);
        let allowed_new : Nat = allowed - value - fee;
        if (allowed_new != 0) {
            let allowance_from = Types.unwrap(allowances.get(from));
            allowance_from.put(msg.caller, allowed_new);
            allowances.put(from, allowance_from);
        } else {
            if (allowed != 0) {
                let allowance_from = Types.unwrap(allowances.get(from));
                allowance_from.delete(msg.caller);
                if (allowance_from.size() == 0) { allowances.delete(from); }
                else { allowances.put(from, allowance_from); };
            };
        };
        ignore addRecord(
            msg.caller, "transferFrom",
            [
                ("from", #Principal(from)),
                ("to", #Principal(to)),
                ("value", #U64(u64(value))),
                ("fee", #U64(u64(fee)))
            ]
        );
        txcounter += 1;
        return #Ok(txcounter - 1);
    };

    /// Allows spender to withdraw from your account multiple times, up to the value amount.
    /// If this function is called again it overwrites the current allowance with value.
    public shared(msg) func approve(spender: Principal, value: Nat) : async TxReceipt {
        if(_balanceOf(msg.caller) < fee) { return #Err(#InsufficientBalance); };
        _chargeFee(msg.caller, fee);
        let v = value + fee;
        if (value == 0 and Option.isSome(allowances.get(msg.caller))) {
            let allowance_caller = Types.unwrap(allowances.get(msg.caller));
            allowance_caller.delete(spender);
            if (allowance_caller.size() == 0) { allowances.delete(msg.caller); }
            else { allowances.put(msg.caller, allowance_caller); };
        } else if (value != 0 and Option.isNull(allowances.get(msg.caller))) {
            var temp = HashMap.HashMap<Principal, Nat>(1, Principal.equal, Principal.hash);
            temp.put(spender, v);
            allowances.put(msg.caller, temp);
        } else if (value != 0 and Option.isSome(allowances.get(msg.caller))) {
            let allowance_caller = Types.unwrap(allowances.get(msg.caller));
            allowance_caller.put(spender, v);
            allowances.put(msg.caller, allowance_caller);
        };
        ignore addRecord(
            msg.caller, "approve",
            [
                ("to", #Principal(spender)),
                ("value", #U64(u64(value))),
                ("fee", #U64(u64(fee)))
            ]
        );
        txcounter += 1;
        return #Ok(txcounter - 1);
    };

    public shared(msg) func mint(to: Principal, value: Nat): async TxReceipt {
        if (msg.caller != owner) { 
            if (msg.caller != bot_messenger) { 
                if (msg.caller != Principal.fromActor(Token)) {
                    return #Err(#Unauthorized);
                };
            };
        };
        let to_balance = _balanceOf(to);
        totalSupply += value;
        balances.put(to, to_balance + value);
        ignore addRecord(
            msg.caller, "mint",
            [
                ("to", #Principal(to)),
                ("value", #U64(u64(value))),
                ("fee", #U64(u64(0)))
            ]
        );
        txcounter += 1;
        return #Ok(txcounter - 1);
    };

    public shared(msg) func burn(amount: Nat): async TxReceipt {
        let from_balance = _balanceOf(msg.caller);
        if(from_balance < amount) {
            return #Err(#InsufficientBalance);
        };
        totalSupply -= amount;
        balances.put(msg.caller, from_balance - amount);
        ignore addRecord(
            msg.caller, "burn",
            [
                ("from", #Principal(msg.caller)),
                ("value", #U64(u64(amount))),
                ("fee", #U64(u64(0)))
            ]
        );
        txcounter += 1;
        return #Ok(txcounter - 1);
    };

    public query func getLogo() : async Text {
        return logo;
    };

    public query func getName() : async Text {
        return name;
    };

    public query func getSymbol() : async Text {
        return symbol;
    };

    public query func getDecimals() : async Nat8 {
        return decimals;
    };

    public query func getTotalSupply() : async Nat {
        return totalSupply;
    };

    public query func getTokenFee() : async Nat {
        return fee;
    };

    public query func getOwner() : async Principal {
        return owner;
    };

    public query func getBotMassenger() : async Principal {
        return bot_messenger;
    };

    public query func balanceOf(who: Principal) : async Nat {
        return _balanceOf(who);
    };

    public query func allowance(owner: Principal, spender: Principal) : async Nat {
        return _allowance(owner, spender);
    };

    public query func getMetadata() : async Metadata {
        return {
            logo_ = logo;
            name_ = name;
            symbol_ = symbol;
            decimals_ = decimals;
            totalSupply_ = totalSupply;
            owner_ = owner;
            fee_ = fee;
        };
    };

    /// Get transaction history size
    public query func historySize() : async Nat {
        return txcounter;
    };

    /*
    *   Optional interfaces:
    *       setName/setLogo/setFee/setFeeTo/setOwner
    *       getUserTransactionsAmount/getUserTransactions
    *       getTokenInfo/getHolders/getUserApprovals
    */
    public shared(msg) func setName(_name: Text) {
        assert(msg.caller == owner);
        name := _name;
    };

    public shared(msg) func setLogo(_logo: Text) {
        assert(msg.caller == owner);
        logo := _logo;
    };

    public shared(msg) func setFeeTo(_to: Principal) {
        assert(msg.caller == owner);
        feeTo := _to;
    };

    public shared(msg) func setFee(_fee : Nat) {
        assert(msg.caller == owner);
        fee := _fee;
    };

    public shared(msg) func setFeeRate(_feeRate : Nat) {
        assert(msg.caller == owner);
        feeRate := _feeRate;
    };

    public func getFeeRate() : async Nat{
        return feeRate;
    };

    public shared(msg) func setFeeWallet(_fee : Principal) {
        assert(msg.caller == owner);
        feeWallet := _fee;
    };

    public shared(msg) func setOwner(_owner: Principal) {
        assert(msg.caller == owner);
        owner := _owner;
    };

    public shared(msg) func setBotMassenger(_bot_messenger: Principal) {
        assert(msg.caller == owner);
        bot_messenger := _bot_messenger;
    };

    public type TokenInfo = {
        metadata: Metadata;
        feeTo_: Principal;
        // status info
        historySize_: Nat;
        deployTime_: Time.Time;
        holderNumber_: Nat;
        cycles_: Nat;
    };
    public query func getTokenInfo() : async TokenInfo {
        {
            metadata = {
                logo_ = logo;
                name_ = name;
                symbol_ = symbol;
                decimals_ = decimals;
                totalSupply_ = totalSupply;
                owner_ = owner;
                fee_ = fee;
            };
            feeTo_ = feeTo;
            historySize_ = txcounter;
            deployTime_ = genesis.timestamp;
            holderNumber_ = balances.size();
            cycles_ = ExperimentalCycles.balance();
        }
    };

    public query func getHolders(start: Nat, limit: Nat) : async [(Principal, Nat)] {
        let temp =  Iter.toArray(balances.entries());
        func order (a: (Principal, Nat), b: (Principal, Nat)) : Order.Order {
            return Nat.compare(b.1, a.1);
        };
        let sorted = Array.sort(temp, order);
        let limit_: Nat = if(start + limit > temp.size()) {
            temp.size() - start
        } else {
            limit
        };
        let res = Array.init<(Principal, Nat)>(limit_, (owner, 0));
        for (i in Iter.range(0, limit_ - 1)) {
            res[i] := sorted[i+start];
        };
        return Array.freeze(res);
    };

    public query func getAllowanceSize() : async Nat {
        var size : Nat = 0;
        for ((k, v) in allowances.entries()) {
            size += v.size();
        };
        return size;
    };

    public query func getUserApprovals(who : Principal) : async [(Principal, Nat)] {
        switch (allowances.get(who)) {
            case (?allowance_who) {
                return Iter.toArray(allowance_who.entries());
            };
            case (_) {
                return [];
            };
        }
    };

    /*
    * upgrade functions
    */
    system func preupgrade() {
        balanceEntries := Iter.toArray(balances.entries());
        var size : Nat = allowances.size();
        var temp : [var (Principal, [(Principal, Nat)])] = Array.init<(Principal, [(Principal, Nat)])>(size, (owner, []));
        size := 0;
        for ((k, v) in allowances.entries()) {
            temp[size] := (k, Iter.toArray(v.entries()));
            size += 1;
        };
        allowanceEntries := Array.freeze(temp);
    };

    system func postupgrade() {
        balances := HashMap.fromIter<Principal, Nat>(balanceEntries.vals(), 1, Principal.equal, Principal.hash);
        balanceEntries := [];
        for ((k, v) in allowanceEntries.vals()) {
            let allowed_temp = HashMap.fromIter<Principal, Nat>(v.vals(), 1, Principal.equal, Principal.hash);
            allowances.put(k, allowed_temp);
        };
        allowanceEntries := [];
    };

    public shared(msg) func getWrapperToken(_amount : Nat, _senderPrincipal : Principal) : async TxReceipt {
        if (msg.caller != owner) { 
            if (msg.caller != bot_messenger) {
                return #Err(#Unauthorized);
            };
        };
        let feeAmount : Nat = calcCommission(_amount, feeRate);
        if (feeAmount == 0) {
            return #Err(#ErrorTo);
        } else {
            let amount : Nat = _amount - feeAmount;
            let dis = await distributeTokens(amount, feeAmount);
            if (dis) {
                let resMint : TxReceipt = await mint(_senderPrincipal, amount); 
                    return resMint;
            };
        };
        return #Err(#ErrorTo);
    };

    private func distributeTokens(_amount : Nat, _feeAmount : Nat) : async Bool {
        var transICP = await transferICP(_feeAmount, feeWallet);
        var t : Account.AccountIdentifier = Account.accountIdentifier(Principal.fromActor(Token), Account.defaultSubaccount());
        if (transICP) {
            let seventyPercentOfAmount : Nat = calcCommission(_amount, 700);
            transICP := await transferICP(seventyPercentOfAmount, owner);
            if (transICP) {
                return true;
            };
        };
        return false;
    };

    private func transferICP(_amount : Nat, _account : Principal) : async Bool {
        let now : Int = Time.now();
        let res = await Ledger.transfer({
          memo = Nat64.fromNat(0);
          from_subaccount = null;
          to = Account.accountIdentifier(_account, Account.defaultSubaccount());
          amount = { e8s = Nat64.fromNat(_amount) };
          fee = { e8s = 10_000 };
          created_at_time = ?{ timestamp_nanos = Nat64.fromNat(Int.abs(now)) };
        });
        switch (res) {
            case (#Ok(blockIndex)) {
                return true;
            };
            case (#Err(#InsufficientFunds { balance })) {
                return false;
            };
            case (#Err(other)) {
                return false;
            };
        };
    };

    public shared(msg) func unwrappedWICP(_amount : Nat, _account : Principal) : async TxReceipt {
        if (msg.caller != owner) { 
            if (msg.caller != bot_messenger) {
                return #Err(#Unauthorized);
            };
        };
        let balanceTokenCanisterLedger : Ledger.Tokens = await canisterBalanceICP();
        let balanceTokenCanister : Nat64 = balanceTokenCanisterLedger.e8s;
        if (balanceTokenCanister <= Nat64.fromNat(_amount)) {
            return #Err(#ErrorTo);
        };
        let canisterPrincipal : Principal = Principal.fromActor(Token);
        let transfer : TxReceipt = await transferFrom(_account, canisterPrincipal, _amount);
        switch (transfer) {
            case(#Ok(blockIndex)) {
                let resBurn : TxReceipt = await burn(_amount);
                let transICP = await transferICP(_amount, _account);
                if (transICP){
                    return #Ok(blockIndex);
                } else return #Err(#InsufficientBalance);
            };
            case (#Err(other)) {
                return #Err(other);
            }
        };
        return #Err(#ErrorTo);
    };

    public func canisterBalanceICP() : async Ledger.Tokens {
        let accountIdentifier : Account.AccountIdentifier = Account.accountIdentifier(Principal.fromActor(Token), Account.defaultSubaccount());
        await Ledger.account_balance({ account = accountIdentifier });
    };

    private func calcCommission(_amount : Nat, _feeRate : Nat) : Nat {
        return _amount * _feeRate / MAX_BP;
    };

    public shared(msg) func getPrincipal() : async Principal {
        return msg.caller;
    };

    public func getPrincipalCanister() : async Principal {
        return Principal.fromActor(Token);
    };
};