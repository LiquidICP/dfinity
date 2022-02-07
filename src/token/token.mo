import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Types "./types";
import Time "mo:base/Time";
import Iter "mo:base/Iter";
import Int "mo:base/Int";
import Array "mo:base/Array";
import Option "mo:base/Option";
import Order "mo:base/Order";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Result "mo:base/Result";
import ExperimentalCycles "mo:base/ExperimentalCycles";
import Debug "mo:base/Debug";
import Error "mo:base/Error";

import Account "./Account";

import Ledger "canister:ledger";

actor Token {
    type Operation = Types.Operation;
    
    type TxReceipt = Result.Result<Nat, {
        #TransactionSuccessful;
        #InsufficientBalance;
        #InsufficientAllowance;
        #Unauthorized;
    }>;

    private stable var owner : Principal = Principal.fromText("aaaaa-aa");
    private stable var bot_messenger : Principal = Principal.fromText("aaaaa-aa");
    private stable var feeWallet : Principal = Principal.fromText("aaaaa-aa");
    private stable var name : Text = "";
    private stable var decimals : Nat = 0;
    private stable var symbol : Text = "";
    private stable var totalSupply : Nat = 0;
    private stable var blackhole : Principal = Principal.fromText("aaaaa-aa");
    private stable var feeRate : Nat = 0;
    private stable var fee : Nat = 0; // transacrion fees
    private stable var isInit = false;
    private stable let MAX_BP = 1000;
    private stable var balanceEntries : [(Principal, Nat)] = [];
    private stable var allowanceEntries : [(Principal, [(Principal, Nat)])] = [];
    private var balances = HashMap.HashMap<Principal, Nat>(1, Principal.equal, Principal.hash);
    private var allowances = HashMap.HashMap<Principal, HashMap.HashMap<Principal, Nat>>(1, Principal.equal, Principal.hash);
    
 
    public shared(msg) func init(
        _name: Text, 
        _symbol: Text,
        _decimals: Nat, 
        _owner: Principal,
        _feeWallet: Principal,
        _bot_messenger: Principal,
        _feeRate: Nat,
        _fee: Nat
        ) {
            assert(isInit == false);
            owner := _owner;
            bot_messenger := _bot_messenger;
            feeWallet := _feeWallet;
            name := _name;
            decimals := _decimals;
            symbol  := _symbol;
            fee := _fee;
            feeRate := _feeRate;
            isInit := true;
        };

    private func _chargeFee(_principalFrom : Principal, _fee : Nat) {
        if (fee > 0) {
            _transfer(_principalFrom, feeWallet, _fee);
        };
    };

    private func _transfer(_from : Principal, _to : Principal, _amount : Nat) {
        let from_balance = _balanceOf(_from);
        let from_balance_new : Nat = from_balance - _amount;
        if (from_balance_new != 0) { balances.put(_from, from_balance_new); }
        else { balances.delete(_from); };

        let to_balance = _balanceOf(_to);
        let to_balance_new : Nat = to_balance + _amount;
        if (to_balance_new != 0) { balances.put(_to, to_balance_new); };
    };

    private func _balanceOf(_account : Principal) : Nat {
        switch (balances.get(_account)) {
            case (?balance) { return balance; };
            case (_) { return 0; };
        }
    };

    private func _allowance(_owner : Principal, _spender : Principal) : Nat {
        switch(allowances.get(_owner)) {
            case (?allowance_owner) {
                switch(allowance_owner.get(_spender)) {
                    case (?allowance) { return allowance; };
                    case (_) { return 0; };
                }
            };
            case (_) { return 0; };
        }
    };


    public shared(msg) func transfer(_to : Principal, _amount : Nat) : async Bool {
        if (_balanceOf(msg.caller) < _amount + fee) { 
            return false; 
        };
        _chargeFee(msg.caller, fee);
        _transfer(msg.caller, _to, _amount);
        return true;
    };

    
    public shared(msg) func transferFrom(_from : Principal, _to : Principal, _amount: Nat) : async Bool {
        if (_balanceOf(_from) < _amount + fee) {
            return false;
        };
        let allowed : Nat = _allowance(_from, msg.caller);
        if (allowed < _amount + fee) {
            return false; 
        };
        _transfer(_from, _to, _amount);
        let allowed_new : Nat = allowed - _amount - fee;
        if (allowed_new != 0) {
            let allowance_from = Types.unwrap(allowances.get(_from));
            allowance_from.put(msg.caller, allowed_new);
            allowances.put(_from, allowance_from);
        } else {
            if (allowed != 0) {
                let allowance_from = Types.unwrap(allowances.get(_from));
                allowance_from.delete(msg.caller);
                if (allowance_from.size() == 0) {
                    allowances.delete(_from); 
                }
                else {
                    allowances.put(_from, allowance_from); 
                };
            };
        };
        return true;
    };

    public shared(msg) func approve(_spender : Principal, _amount : Nat) : async Bool {
        if(_balanceOf(msg.caller) < fee) { 
                return false; 
            };
        _chargeFee(msg.caller, fee);
        let v = _amount + fee;
        if (_amount == 0 and Option.isSome(allowances.get(msg.caller))) {
            let allowance_caller = Types.unwrap(allowances.get(msg.caller));
            allowance_caller.delete(_spender);
            if (allowance_caller.size() == 0) { allowances.delete(msg.caller); }
            else { allowances.put(msg.caller, allowance_caller); };
        } else if (_amount != 0 and Option.isNull(allowances.get(msg.caller))) {
            var temp = HashMap.HashMap<Principal, Nat>(1, Principal.equal, Principal.hash);
            temp.put(_spender, v);
            allowances.put(msg.caller, temp);
        } else if (_amount != 0 and Option.isSome(allowances.get(msg.caller))) {
            let allowance_caller = Types.unwrap(allowances.get(msg.caller));
            allowance_caller.put(_spender, v);
            allowances.put(msg.caller, allowance_caller);
        };
        return true;
    };

    public shared(msg) func mint(_principalTo: Principal, _amount : Nat) : async Bool {
        if(msg.caller != owner) { 
            if (msg.caller != bot_messenger) { 
                if(msg.caller != Principal.fromActor(Token)) {
                    return false;
                };
            };
        };
        let to_balance = _balanceOf(_principalTo);
        totalSupply += _amount;
        balances.put(_principalTo, to_balance + _amount);
        return true;
    };

    public shared(msg) func burn(_amount : Nat): async Bool {
        let from_balance = _balanceOf(msg.caller);
        if(from_balance < _amount) {
            return false;
        };
        totalSupply -= _amount;
        balances.put(msg.caller, from_balance - _amount);
        return true;
    };

    public query func getName() : async Text {
        return name;
    };

    public query func getSymbol() : async Text {
        return symbol;
    };

    public query func getDecimals() : async Nat {
        return decimals;
    };

    public query func getTotalSupply() : async Nat {
        return totalSupply;
    };

    public query func getTokenFee() : async Nat {
        return fee;
    };

    public query func getTokenFeeRate() : async Nat {
        return feeRate;
    };

    public query func balanceOf(_principal : Principal) : async Nat {
        return _balanceOf(_principal);
    };

    public query func allowance(_owner : Principal, _spender : Principal) : async Nat {
        return _allowance(_owner, _spender);
    };

    public shared(msg) func setFeeWallet(_fee : Principal) {
        assert(msg.caller == owner);
        feeWallet := _fee;
    };

    public shared(msg) func setFee(_fee : Nat) {
        assert(msg.caller == owner);
        fee := _fee;
    };

    public shared(msg) func setFeeRate(_feeRate : Nat) {
        assert(msg.caller == owner);
        feeRate := _feeRate;
    };

    public shared(msg) func setOwner(_owner : Principal) {
        assert(msg.caller == owner);
        owner := _owner;
    };

    public shared(msg) func getWrapperToken(_amount : Nat, _senderPrincipal : Principal) : async Bool {
        if (msg.caller != owner) {
            if (msg.caller != bot_messenger) { 
                return false;
            };
        };
        let feeAmount : Nat = calcCommission(_amount, feeRate);
        let amount : Nat = _amount - feeAmount;
        let dis = await distributeTokens(amount, feeAmount);
        if (dis) {
            let resMint : Bool = await mint(_senderPrincipal, amount); 
            if (resMint) {
                return true;
            };
        };
        return false;
    };

    private func distributeTokens(_amount : Nat, _feeAmount : Nat) : async Bool {
        var transICP = await transferICP(_feeAmount, feeWallet);
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
            Debug.print("Paid reward to " # debug_show _account # " in block " # debug_show blockIndex);
            return true;
          };
          case (#Err(#InsufficientFunds { balance })) {
            throw Error.reject("The balance is only " # debug_show balance # " e8s");
            return false;
          };
          case (#Err(other)) {
            throw Error.reject("Unexpected error: " # debug_show other);
            return false;
          };
        };
    };

    public shared(msg) func unwrappedWICP(_amount : Nat, _account : Principal) : async Bool {
        if (msg.caller != owner) {
            if (msg.caller != bot_messenger) {
                return false;
            };
        };
        let balanceTokenCanisterLedger : Ledger.Tokens = await canisterBalanceICP();
        let balanceTokenCanister : Nat64 = balanceTokenCanisterLedger.e8s;
        if (balanceTokenCanister <= Nat64.fromNat(_amount)) {
            return false;
        };
        let canisterPrincipal : Principal = Principal.fromActor(Token);
        let transICP = await transferICP(_amount, _account);
        if (transICP) {
            let transfer = await transferFrom(_account, canisterPrincipal, _amount);
            if (transfer) {
                let resBurn : Bool = await burn(_amount);
                if (resBurn) {
                    return true;
                };
            };
        };
        return false;
    };

    public func canisterBalanceICP() : async Ledger.Tokens {
        let accountIdentifier : Account.AccountIdentifier = Account.accountIdentifier(Principal.fromActor(Token), Account.defaultSubaccount());
        await Ledger.account_balance({ account = accountIdentifier });
    };

    private func calcCommission(_amount : Nat, _feeRate : Nat) : Nat {
        return _amount * _feeRate / MAX_BP;
    };

};