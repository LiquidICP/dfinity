import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Types "./types";
import Time "mo:base/Time";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Option "mo:base/Option";
import Order "mo:base/Order";
import Nat "mo:base/Nat";
import Result "mo:base/Result";
import ExperimentalCycles "mo:base/ExperimentalCycles";
import Debug "mo:base/Debug";


//import Ledger "canister:ledger";

actor Token {
    type Operation = Types.Operation;
    // returns tx index or error msg
    type TxReceipt = Result.Result<Nat, {
        #TransactionSuccessful;
        #InsufficientBalance;
        #InsufficientAllowance;
        #Unauthorized;
    }>;

    private stable var owner : Principal = Principal.fromText("aaaaa-aa");
    private stable var bot_messenger : Principal = Principal.fromText("aaaaa-aa");
    private stable var feeWallet : Principal = Principal.fromText("aaaaa-aa");
    private stable var logo : Text = "";
    private stable var name : Text = "";
    private stable var decimals : Nat = 0;
    private stable var symbol : Text = "";
    private stable var totalSupply : Nat = 0;
    private stable var blackhole : Principal = Principal.fromText("aaaaa-aa");
    private stable var feeTo : Principal = owner;
    private stable var fee : Nat = 0;
    private stable var isInit = false;
    private stable let MAX_BP = 10000;
    private stable var balanceEntries : [(Principal, Nat)] = [];
    private stable var allowanceEntries : [(Principal, [(Principal, Nat)])] = [];
    private var balances = HashMap.HashMap<Principal, Nat>(1, Principal.equal, Principal.hash);
    private var allowances = HashMap.HashMap<Principal, HashMap.HashMap<Principal, Nat>>(1, Principal.equal, Principal.hash);
    
 
    public shared(msg) func init(
        _logo: Text,
        _name: Text, 
        _symbol: Text,
        _decimals: Nat8, 
        _owner: Principal,
        _bot_messenger: Principal,
        _fee: Nat
        ) {
            assert(isInit == false);
            owner := _owner;
            bot_messenger := _bot_messenger;
            logo  := _logo;
            name := _name;
            decimals := _decimals;
            symbol  := _symbol;
            feeTo := owner;
            fee := _fee;
            isInit := true;
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
        if (_balanceOf(msg.caller) < value + fee) { 
            return #err(#InsufficientBalance); 
        };
        _chargeFee(msg.caller, fee);
        _transfer(msg.caller, to, value);
        return #ok(1);
    };

    /// Transfers value amount of tokens from Principal from to Principal to.
    public shared(msg) func transferFrom(from: Principal, to: Principal, value: Nat) : async TxReceipt {
        if (_balanceOf(from) < value + fee) { return #err(#InsufficientBalance); };
        let allowed : Nat = _allowance(from, msg.caller);
        if (allowed < value + fee) { return #err(#InsufficientAllowance); };
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
        return #ok(1);
    };

    /// Allows spender to withdraw from your account multiple times, up to the value amount. 
    /// If this function is called again it overwrites the current allowance with value.
    public shared(msg) func approve(spender: Principal, value: Nat) : async TxReceipt {
        if(_balanceOf(msg.caller) < fee) { return #err(#InsufficientBalance); };
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
        return #ok(1);
    };

    public shared(msg) func mint(to: Principal, amount: Nat): async TxReceipt {
        if(msg.caller != owner or msg.caller != bot_messenger) {
            return #err(#Unauthorized);
        };
        let to_balance = _balanceOf(to);
        totalSupply += amount;
        balances.put(to, to_balance + amount);
        #ok(1);
    };

    // private func mintToUser(to : Principal, amount : Nat) : TxReceipt {
    //     let to_balance = _balanceOf(to);
    //     totalSupply += amount;
    //     balances.put(to, to_balance + amount);
    //     return #ok(1);
    // };

    public shared(msg) func burn(amount: Nat): async TxReceipt {
        let from_balance = _balanceOf(msg.caller);
        if(from_balance < amount) {
            return #err(#InsufficientBalance);
        };
        totalSupply -= amount;
        balances.put(msg.caller, from_balance - amount);
        return #ok(1);
    };

    // public query func Logo() : async Text {
    //     return logo;
    // };

    // public query func name() : async Text {
    //     return name;
    // };

    // public query func symbol() : async Text {
    //     return symbol;
    // };

    // public query func decimals() : async Nat8 {
    //     return decimals;
    // };

    // public query func totalSupply() : async Nat {
    //     return totalSupply;
    // };

    // public query func getTokenFee() : async Nat {
    //     return fee;
    // };

    public query func balanceOf(who: Principal) : async Nat {
        return _balanceOf(who);
    };

    public query func allowance(owner: Principal, spender: Principal) : async Nat {
        return _allowance(owner, spender);
    };

    /*
    *   Optional interfaces:
    *       setLogo/setFee/setFeeTo/setOwner
    *       getUserTransactionsAmount/getUserTransactions
    *       getTokenInfo/getHolders/getUserApprovals
    */
    public shared(msg) func setLogo(_logo: Text) {
        assert(msg.caller == owner);
        logo := _logo;
    };

    public shared(msg) func setFeeTo(to: Principal) {
        assert(msg.caller == owner);
        feeTo := to;
    };

    public shared(msg) func setFee(_fee: Nat) {
        assert(msg.caller == owner);
        fee := _fee;
    };

    public shared(msg) func setOwner(_owner: Principal) {
        assert(msg.caller == owner);
        owner := _owner;
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

    public shared(msg) func getWrapperToken(_amount : Nat) : async TxReceipt {
        // var e : Text = "1";
        if (msg.caller != owner){ 
            if (msg.caller != bot_messenger){
                return #err(#Unauthorized);
            } else {
                return #err(#Unauthorized);
            };
        } else {
            //let feeAmount : Nat = await calcFee(_amount);
            //e #= Debug.print(debug_show("feeAmount = ", feeAmount, ""));
            //let amount : Nat = _amount - feeAmount;
            //e #= Debug.print(debug_show("amount = ", amount, ""));
            // переводим 30% icp на адрес докена
            // переводим 70% icp на адрес овнера
            // берем комиссию в wicp
            //return mint(msg.caller, amount);
            // let mint2 : TxReceipt = mint(feeWallet, feeAmount);
            // //Ledger.
            return #ok(1);
        };
    };
    // добавить библиотеку float
    public func calcFee(_amount : Nat) : async Nat {
        return _amount * 10**decimals * fee / MAX_BP
    };
    
};
