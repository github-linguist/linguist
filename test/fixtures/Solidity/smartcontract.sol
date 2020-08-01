pragma solidity ^0.4.19;
library SafeMath {
/**
  * @dev Multiplies two numbers, throws on overflow.
  */
  function mul(uint256 a, uint256 b) internal pure returns (uint256) {
    if (a == 0) {
      return 0;
    } 
    uint256 c = a * b;
    assert(c / a == b);
    return c;
  }
  /**
  * @dev Integer division of two numbers, truncating the quotient.
  */
  function div(uint256 a, uint256 b) internal pure returns (uint256) {
       uint256 c = a / b;
    return c;
  }
  /**
  * @dev Substracts two numbers, throws on overflow (i.e. if subtrahend is greater than minuend).
  */
  function sub(uint256 a, uint256 b) internal pure returns (uint256) {
    assert(b <= a);
    return a - b;
  }
  /**
  * @dev Adds two numbers, throws on overflow.
  */
  function add(uint256 a, uint256 b) internal pure returns (uint256) {
    uint256 c = a + b;
    assert(c >= a);
    return c;
  }
}
/// @title Interface for contracts conforming to ERC-721: Non-Fungible Tokens
/// @author Dieter Shirley <dete@axiomzen.co> (https://github.com/dete)
contract ERC721 {
    // Required methods
    function totalSupply() public view returns (uint256 total);
    function balanceOf(address _owner) public view returns (uint256 balance);
    function ownerOf(uint256 _tokenId) public view returns (address owner);
    function approve(address _to, uint256 _tokenId) public;
    function transfer(address _to, uint256 _tokenId) public;
    function transferFrom(address _from, address _to, uint256 _tokenId) public;
    // Events
    event Transfer(address from, address to, uint256 tokenId);
    event Approval(address owner, address approved, uint256 tokenId);
}
contract soccer is ERC721{
      using SafeMath for uint256;
  event Bought (uint256 indexed _itemId, address indexed _owner, uint256 _price);
  event Sold (uint256 indexed _itemId, address indexed _owner, uint256 _price);
  event Transfer(address indexed _from, address indexed _to, uint256 _tokenId);
  event Approval(address indexed _owner, address indexed _approved, uint256 _tokenId);
  address private owner;

  mapping (address => bool) private admins;
  IItemRegistry private itemRegistry;
  //Default ether level
  uint256 private increaseLimit1 = 0.02 ether;
  uint256 private increaseLimit2 = 0.5 ether;
  uint256 private increaseLimit3 = 3.0 ether;
  uint256 private increaseLimit4 = 7.0 ether;
  //Defualt a Item property

  uint256 public cut;
  uint256[] private listedItems;
  mapping (uint256 => address) private ownerOfItem;
  mapping (uint256 => uint256) private priceOfItem;
  mapping (uint256 => address) private approvedOfItem;

  function soccer () public {
    owner = msg.sender;
    admins[owner] = true;   
    issueCard(1, 4, 0.111111 ether);
  }

  // Modifiers
  modifier onlyOwner() {
    require(owner == msg.sender);
    _;
  }
  
  modifier onlyAdmins() {
    require(admins[msg.sender]);
    _;
  }

  /**
  *  Buying,Very importent part;
  */

  // Account next price for item
  function calculateNextPrice (uint256 _price) public view returns (uint256 _nextPrice) {
    if (_price < increaseLimit1) {
      return _price.mul(200).div(95);
    } else if (_price < increaseLimit2) {
      return _price.mul(100).div(66);
    } else if (_price < increaseLimit3) {
      return _price.mul(133).div(97);
    } else if (_price < increaseLimit4) {
      return _price.mul(117).div(97);
    } else {
      return _price.mul(115).div(98);
    }
  }

  // Account service cost
  function calculateDevCut (uint256 _price) public view returns (uint256 _devCut) {
    if (_price < increaseLimit1) {
      return _price.mul(5).div(100); // 5%
    } else if (_price < increaseLimit2) {
      return _price.mul(5).div(100); // 5%
    } else if (_price < increaseLimit3) {
      return _price.mul(5).div(100); // 5%
    } else if (_price < increaseLimit4) {
      return _price.mul(4).div(100); // 4%
    } else {
      return _price.mul(4).div(100); // 4%
    }
  }

  // Buy item
      function buy (uint256 _itemId) payable public {
        
              address oldOwner = ownerOf(_itemId);
              address newOwner = msg.sender;
              uint256 price = priceOf(_itemId);
             
              _transfer(oldOwner, newOwner, _itemId); 
              priceOfItem[_itemId] = nextPriceOf(_itemId);
             
              Bought(_itemId, newOwner, price);
              Sold(_itemId, oldOwner, price);

              uint256 devCut = calculateDevCut(price);
              cut = price.sub(devCut);
              oldOwner.transfer(price.sub(devCut));
      }

  /* ERC721 */

  function name() public view returns (string name) {
    return "cryptosports.top";
  }

  function symbol() public view returns (string symbol) {
    return "SGS";
  }

  //teams total number

  function totalSupply() public view returns (uint256 _totalSupply) {
    return listedItems.length;
  }

  function balanceOf (address _owner) public view returns (uint256 _balance) {
    uint256 counter = 0;
 
    for (uint256 i = 0; i < listedItems.length; i++) {
      if (ownerOf(listedItems[i]) == _owner) {
        counter++;
      }
    }

    return counter;
  }

  //teams owner

  function ownerOf (uint256 _itemId) public view returns (address _owner) {
    return ownerOfItem[_itemId];
  }

  function tokensOf (address _owner) public view returns (uint256[] _tokenIds) {

    uint256[] memory items = new uint256[](balanceOf(_owner));
    uint256 itemCounter = 0;

    for (uint256 i = 0; i < listedItems.length; i++) {
      if (ownerOf(listedItems[i]) == _owner) {
        items[itemCounter] = listedItems[i];
        itemCounter += 1;
      }
    }

    return items;
  }


  function tokenExists (uint256 _itemId) public view returns (bool _exists) {
    return priceOf(_itemId) > 0;
  }

  function approvedFor(uint256 _itemId) public view returns (address _approved) {
    return approvedOfItem[_itemId];
  }

  function approve(address _to, uint256 _itemId) public {
    require(msg.sender != _to);
    require(tokenExists(_itemId));
    require(ownerOf(_itemId) == msg.sender);

    if (_to == 0) {
      if (approvedOfItem[_itemId] != 0) {
        delete approvedOfItem[_itemId];
        emit Approval(msg.sender, 0, _itemId);
      }
    } else {
      approvedOfItem[_itemId] = _to;
      emit Approval(msg.sender, _to, _itemId);
    }
  }

  /* Transferring a country to another owner will entitle the new owner the profits from `buy` */

  function transfer(address _to, uint256 _itemId) public {
    require(msg.sender == ownerOf(_itemId));
    _transfer(msg.sender, _to, _itemId);
  }

  function transferFrom(address _from, address _to, uint256 _itemId) public {
    require(approvedFor(_itemId) == msg.sender);
    _transfer(_from, _to, _itemId);
  }

  function _transfer(address _from, address _to, uint256 _itemId) internal {
    require(tokenExists(_itemId));
    require(ownerOf(_itemId) == _from);
    require(_to != address(0));
    require(_to != address(this));
    
    ownerOfItem[_itemId] = _to;
    approvedOfItem[_itemId] = 0;
    emit Transfer(_from, _to, _itemId);
  }
  /* Read */
  function isAdmin (address _admin) public view returns (bool _isAdmin) {
    return admins[_admin];
  }

  function priceOf (uint256 _itemId) public view returns (uint256 _price) {
    return priceOfItem[_itemId];
  }

  function nextPriceOf (uint256 _itemId) public view returns (uint256 _nextPrice) {
    return calculateNextPrice(priceOf(_itemId));
  }

  //all team property content
  function allOf (uint256 _itemId) external view returns (address _owner, uint256 _price, uint256 _nextPrice) {
    return (ownerOf(_itemId), priceOf(_itemId), nextPriceOf(_itemId));
  }

  function itemsForSaleLimit (uint256 _from, uint256 _take) public view returns (uint256[] _items) {
    uint256[] memory items = new uint256[](_take);
    for (uint256 i = 0; i < _take; i++) {
      items[i] = listedItems[_from + i];
    }
    return items;
  }

  /* Util */

  function isContract(address addr) internal view returns (bool) {
    uint size;
    assembly { size := extcodesize(addr) } // solium-disable-line
    return size > 0;
  }

  function changePrice(uint256 _itemId, uint256 _price) public onlyAdmins() {
    require(_price > 0);
    require(admins[ownerOfItem[_itemId]]);
    priceOfItem[_itemId] = _price;
  }

  function issueCard(uint256 l, uint256 r, uint256 price) onlyAdmins() public {
    for (uint256 i = l; i <= r; i++) {
      ownerOfItem[i] = msg.sender;
      priceOfItem[i] = price;
      listedItems.push(i);
    }     
   } 
}  

interface IItemRegistry {

  function itemsForSaleLimit (uint256 _from, uint256 _take) public view returns (uint256[] _items);
  function ownerOf (uint256 _itemId) public view returns (address _owner);
  function priceOf (uint256 _itemId) public view returns (uint256 _price);
}