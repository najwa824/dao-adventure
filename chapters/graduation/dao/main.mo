import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Types "types";
import Buffer "mo:base/Buffer";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Nat64 = "mo:base/Nat64";
import Nat32 "mo:base/Nat32";
import Option "mo:base/Option";
import Hash "mo:base/Hash";
import Time "mo:base/Time";
import Map "mo:map/Map";
import Array "mo:base/Array";
import Debug "mo:base/Debug";

actor {

    type Result<A, B> = Result.Result<A, B>;
    type Member = Types.Member;
    type ProposalContent = Types.ProposalContent;
    type ProposalId = Types.ProposalId;
    type Proposal = Types.Proposal;
    type Vote = Types.Vote;
    type HttpRequest = Types.HttpRequest;
    type HttpResponse = Types.HttpResponse;
    type HashMap<K, V> = Types.HashMap<K, V>;

    let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);
    var nextProposalId: Nat = 0;
    let proposals = HashMap.HashMap<ProposalId, Proposal>(0, Nat.equal, Hash.hash);

    let admin: Member = {
        name = "motoko_bootcamp";
        role = #Mentor;
        tokens = 10;
    };
    members.put(Principal.fromText("nkqop-siaaa-aaaaj-qa3qq-cai"), admin);



    let graduation_token = actor("jaamb-mqaaa-aaaaj-qa3ka-cai") : actor {
        mint : shared (owner : Principal, amount : Nat) -> async Result<(), Text>;
        burn : shared (owner : Principal, amount : Nat) -> async Result<(), Text>;
    };

    // The principal of the Webpage canister associated with this DAO canister (needs to be updated with the ID of your Webpage canister)
    stable let canisterIdWebpage: Principal = Principal.fromText("zrakb-eaaaa-aaaab-qacaq-cai");
    stable var manifesto = "lets graduate!!!";
    stable let name = "Motoko Bootcamp";
    //stable var goals = [];
    let goals = Buffer.Buffer<Text>(0);

    let webPage = actor("zrakb-eaaaa-aaaab-qacaq-cai") : actor {
        setManifesto : shared (newManifesto : Text) -> async Result<(), Text>;
    };

    // Returns the name of the DAO
    public query func getName() : async Text {
        return name;
    };

    // Returns the manifesto of the DAO
    public query func getManifesto() : async Text {
        return manifesto;
    };

    // Returns the goals of the DAO
    public query func getGoals() : async [Text] {
        //return goals;
        return Buffer.toArray(goals);
    };

    // Register a new member in the DAO with the given name and principal of the caller
    // Airdrop 10 MBC tokens to the new member
    // New members are always Student
    // Returns an error if the member already exists
    public shared ({ caller }) func registerMember(member : Member) : async Result<(), Text> {
        switch(members.get(caller)) {
            case(null) {
                let newMember: Member = {
                    name = member.name;
                    role = #Student;
                    tokens = 10;
                };
                members.put(caller, newMember);
                switch(await graduation_token.mint(caller, 10)) {
                    case(#ok()) {
                        return #ok();
                    };
                    case(#err(error)) {
                        return #err(error);
                    };
                };
                return #ok();
            };
            case(? member) {
                return #err("member already exists!");
            };
        };
    };

    // Get the member with the given principal
    // Returns an error if the member does not exist
    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch (members.get(p)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (? member) {
                return #ok(member);
            };
        };
    };

    // Graduate the student with the given principal
    // Returns an error if the student does not exist or is not a student
    // Returns an error if the caller is not a mentor
    public shared ({ caller }) func graduate(student : Principal) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (? member) {
                switch(member.role) {
                    case(#Mentor) { 
                        return #err("the member is already a mentor!");
                    };
                    case(#Graduate) {
                        return #err("the member is already a graduate!");
                    };
                    case(#Student) {
                        // Upgrade to graduate
                        let newGraduate: Member = {
                            name = member.name;
                            role = #Graduate;
                            tokens = member.tokens;
                        };
                        members.put(caller, newGraduate);
                        return #ok();
                    };
                };
            };
        };
    };

    // Create a new proposal and return its id
    // Returns an error if the caller is not a mentor or doesn't own at least 1 MBC token
    public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
    // Ensure the caller is a member and has enough tokens
    switch(members.get(caller)) {
        case(null) { 
            return #err("The caller is not a member!");
        };
        case(? member) {
            if (member.tokens < 1) {
                return #err("Insufficient balance to create a proposal!");
            };
            if (member.role != #Mentor) {
                return #err("The member is not a Mentor!");
            };

            // Handle different types of proposals
            switch(content) {
                case (#ChangeManifesto(newManifesto)) {
                    // Create a proposal to change the manifesto
                    let newProposal: Proposal = {
                        id = nextProposalId;
                        content = #ChangeManifesto(newManifesto);  // The content contains the new manifesto text
                        creator = caller;
                        created = Time.now();
                        executed = null;
                        votes = [];
                        voteScore = 0;
                        status = #Open;
                    };
                    proposals.put(nextProposalId, newProposal);
                    
                    // Burn 1 token to create the proposal
                    switch(await graduation_token.burn(caller, 1)) {
                        case(#ok()) {
                            nextProposalId += 1;
                            return #ok(nextProposalId - 1);  // Return the created proposal ID
                        };
                        case(#err(error)) {
                            return #err(error);
                        };
                    };
                };
                
                case (#AddMentor(newMentor)) {
                    // Ensure the new mentor is a graduate
                    switch(members.get(newMentor)) {
                        case (null) {
                            return #err("The specified principal is not a member!");
                        };
                        case (? newMember) {
                            if (newMember.role != #Graduate) {
                                return #err("The specified principal is not a Graduate and cannot be promoted to a Mentor!");
                            };
                        };
                    };
                    
                    // Create a proposal to add a mentor
                    let newProposal: Proposal = {
                        id = nextProposalId;
                        content = #AddMentor(newMentor); 
                        creator = caller;
                        created = Time.now();
                        executed = null;
                        votes = [];
                        voteScore = 0;
                        status = #Open;
                    };
                    proposals.put(nextProposalId, newProposal);
                    
                    // Burn 1 token to create the proposal
                    switch(await graduation_token.burn(caller, 1)) {
                        case(#ok()) {
                            nextProposalId += 1;
                            return #ok(nextProposalId - 1);  // Return the created proposal ID
                        };
                        case(#err(error)) {
                            return #err(error);
                        };
                    };
                };
            };
        };
    };
};


    // Get the proposal with the given id
    // Returns an error if the proposal does not exist
    public query func getProposal(id : ProposalId) : async Result<Proposal, Text> {
        switch(proposals.get(id)) {
            case(null) { 
                return #err("Proposal doesn't exist!");
            };
            case(? proposal) {
                return #ok(proposal);
            };
        };
    };

    // Returns all the proposals
    public query func getAllProposals() : async [Proposal] {
        return Iter.toArray(proposals.vals());
    };

    // Vote for the given proposal
    // Returns an error if the proposal does not exist or the member is not allowed to vote
    public shared ({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
        switch(members.get(caller)) {
            case(null) {
                return #err("The caller is not a member - cannot vote on proposal");
            };
            case(? member) {
                switch(proposals.get(proposalId)) {
                    case(null) {
                        return #err("The proposal does not exist");
                    };
                    case(? proposal) {
                        if (proposal.status != #Open) {
                            return #err("The proposal is not open for voting");
                        };
                        if (_hasVoted(proposal, caller)) {
                            return #err("The caller has already voted on this proposal");
                        };
                        if (member.role == #Student) {
                            return #err("Students can't vote!");
                        };

                        let balance = member.tokens;
                        let multiplierVote = if (yesOrNo) { 1 } else { -1 };

                        let newVoteScore = proposal.voteScore + (if (member.role == #Mentor) { 5 * balance } else { balance }) * multiplierVote;
                        var newExecuted : ?Time.Time = null;
                        let newStatus = if (newVoteScore >= 100) {
                            #Accepted;
                        } else if (newVoteScore <= -100) {
                            #Rejected;
                        } else {
                            #Open;
                        };

                        let newVotes = Buffer.fromArray<Vote>(proposal.votes);
                        switch (newStatus) {
                            case(#Accepted) {
                                await _executeProposal(proposal.content);
                                newExecuted := ?Time.now();
                            };
                            case(_) {};
                        };

                        let updatedProposal: Proposal = {
                            id = proposal.id;
                            content = proposal.content;
                            creator = proposal.creator;
                            created = proposal.created;
                            executed = newExecuted;
                            votes = Buffer.toArray(newVotes);
                            voteScore = newVoteScore;
                            status = newStatus;
                        };
                        proposals.put(proposal.id, updatedProposal);
                        return #ok();
                    };
                };
            };
        };
    };

    func _hasVoted(proposal : Proposal, member : Principal) : Bool {
        for (vote in proposal.votes.vals()) {
            if (vote.member == member) {
                return true;
            };
        };
        return false;
    };

    func _executeProposal(content : ProposalContent) : async () {
    switch (content) {
        case (#ChangeManifesto(newManifesto)) {
            manifesto := newManifesto;
            Debug.print("Dao manifesto updated to: " # newManifesto);

            switch(await webPage.setManifesto(newManifesto)) {
                case(#ok()) {
                    Debug.print("Webpage manifesto updated to: " # newManifesto);
                    return;
                };
                case(#err(error)) {
                    return;
                };
            };
        };
        case (#AddMentor(newMentor)) {
            // Upgrade the role of the member to Mentor
            switch(members.get(newMentor)) {
                case (null) {
                    return;
                };
                case (? member) {
                    // Check if the member is already a Mentor
                    if (member.role == #Mentor) {
                        return; // If already a mentor, do nothing
                    };
                    if (member.role == #Student) {
                        return; // the students can not be a mentor without graduation!
                    };
                    // Otherwise, upgrade the role to Mentor
                    let newMember: Member = {
                        name = member.name;
                        role = #Mentor;
                        tokens = member.tokens;
                    };
                    // Update the member in the `members` map
                    members.put(newMentor, newMember);
                };
            };
        };
    };
    return;
};


    // Returns the Principal ID of the Webpage canister associated with this DAO canister
    public query func getIdWebpage() : async Principal {
        return canisterIdWebpage;
    };
};
