pragma solidity ^0.4.10;
contract LherbeurContract {
    
    address public owner;
    bytes32 dapChoice;
    bytes32 expectedRole;
    uint yearsOfExperience;
    bytes32 progLang;
    bytes32 assetQuality;
    bytes32 whatIsSuperDAO;
    
    function LherbeurContract()
    {
        owner = msg.sender;
    }
    
    // function checkOwner(address executor) returns (bool)
    // {
    //     if (executor == owner)
    //     return true;
    //     else 
    //     return false;
    // }
    
    modifier accessGrant(address _executor)
    {
        if (_executor != owner)
            throw;
        
        _;
    }
    
    function setdapChoice(bytes32 dap) 
        accessGrant (owner)
    { 
        dapChoice = dap; 
    }
    
    function getdapChoice() 
        accessGrant (owner)
        returns(bytes32) 
    { 
        return dapChoice; 
        
    }
    
    function setexpectedRole(bytes32 role) 
        accessGrant (owner)
    { 
        expectedRole = role; 
    }
    
    function getexpectedRole() 
        accessGrant (owner)
        returns(bytes32) 
    { 
        return expectedRole; 
        
    }

 
    function setyearsOfExperience(uint yearsExp) 
        accessGrant (owner)
    { 
        yearsOfExperience = yearsExp; 
    }
    
    function getyearsOfExperience() 
        accessGrant (owner)
        returns(uint) 
    { 
        return yearsOfExperience; 
        
    }

    
    function setprogLang(bytes32 lang) 
        accessGrant (owner)
    { 
        progLang = lang; 
    }
    
    function getprogLang() 
        accessGrant (owner)
        returns(bytes32) 
    { 
        return progLang; 
        
    }
    
     
    function setassetQuality(bytes32 quality) 
        accessGrant (owner)
    { 
        assetQuality = quality; 
    }
    
    function getassetQuality() 
        accessGrant (owner)
        returns(bytes32) 
    { 
        return assetQuality; 
        
    }
    
     
    function setwhatIsSuperDAO(bytes32 superDAOWhat) 
        accessGrant (owner)
    { 
        whatIsSuperDAO = superDAOWhat; 
    }
    
    function getwhatIsSuperDAO() 
        accessGrant (owner)
        returns(bytes32) 
    { 
        return whatIsSuperDAO; 
        
    }
    
}
