pragma solidity 0.5.1;

import "./HitchensUnorderedKeySet.sol";

contract Widget {
    
    using HitchensUnorderedKeySetLib for HitchensUnorderedKeySetLib.Set;
    HitchensUnorderedKeySetLib.Set widgetSet;
    
    struct WidgetStruct {
        string name;
        bool delux;
        uint price;
    }
    
    mapping(bytes32 => WidgetStruct) widgets;
    
    event LogNewWidget(address sender, bytes32 key, string name, bool delux, uint price);
    event LogUpdateWidget(address sender, bytes32 key, string name, bool delux, uint price);    
    event LogRemWidget(address sender, bytes32 key);
    
    function newWidget(bytes32 key, string memory name, bool delux, uint price) public {
        widgetSet.insert(key); // Note that this will fail automatically if the key already exists.
        WidgetStruct storage w = widgets[key];
        w.name = name;
        w.delux = delux;
        w.price = price;
        emit LogNewWidget(msg.sender, key, name, delux, price);
    }
    
    function updateWidget(bytes32 key, string memory name, bool delux, uint price) public {
        require(widgetSet.exists(key), "Can't update a widget that doesn't exist.");
        WidgetStruct storage w = widgets[key];
        w.name = name;
        w.delux = delux;
        w.price = price;
        emit LogUpdateWidget(msg.sender, key, name, delux, price);
    }
    
    function remWidget(bytes32 key) public {
        widgetSet.remove(key); // Note that this will fail automatically if the key doesn't exist
        delete widgets[key];
        emit LogRemWidget(msg.sender, key);
    }
    
    function getWidget(bytes32 key) public view returns(string memory name, bool delux, uint price) {
        require(widgetSet.exists(key), "Can't get a widget that doesn't exist.");
        WidgetStruct storage w = widgets[key];
        return(w.name, w.delux, w.price);
    }
    
    function getWidgetCount() public view returns(uint count) {
        return widgetSet.count();
    }
    
    function getWidgetAtIndex(uint index) public view returns(bytes32 key) {
        return widgetSet.keyAtIndex(index);
    }
}
