import Types "types";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Debug "mo:base/Debug";
actor Webpage {

    type Result<A, B> = Result.Result<A, B>;
    type HttpRequest = Types.HttpRequest;
    type HttpResponse = Types.HttpResponse;

    // The manifesto stored in the webpage canister should always be the same as the one stored in the DAO canister
    stable var manifesto : Text = "lets graduate!!!";

    // The webpage displays the manifesto
    public query func http_request(request : HttpRequest) : async HttpResponse {
        return ({
            status_code = 404;
            headers = [];
            body = Text.encodeUtf8("Hello world!");
            streaming_strategy = null;
        });
    };

    // This function should only be callable by the DAO canister (no one else should be able to change the manifesto)
    public shared ({ caller }) func setManifesto(newManifesto : Text) : async Result<(), Text> {
        let daoCanister = Principal.fromText("x5pps-pqaaa-aaaab-qadbq-cai");
        switch(Principal.equal(caller, daoCanister)) {
            case(true) { 
                manifesto := newManifesto;
                Debug.print("Webpage manifesto updated to: " # newManifesto);
                return #ok();
            };
            case(false) {
                return #err("the caller is not the dao canister!");
            };
        };
    };
};
