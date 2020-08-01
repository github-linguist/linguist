pragma solidity 0.5.1;

import "./HitchensUnorderedAddressSet.sol";

contract Widget {
    
    using HitchensUnorderedAddressSetLib for HitchensUnorderedAddressSetLib.Set;
    HitchensUnorderedAddressSetLib.Set widgetSet;
    
    struct WidgetStruct {
        string name;
        bool delux;
        uint price;
    }
    
    mapping(address => WidgetStruct) widgets;
    
    event LogNewWidget(address sender, address key, string name, bool delux, uint price);
    event LogUpdateWidget(address sender, address key, string name, bool delux, uint price);    
    event LogRemWidget(address sender, address key);
    
    function newWidget(address key, string memory name, bool delux, uint price) public { 
        widgetSet.insert(key); // Note that this will fail automatically if the key already exists.
        WidgetStruct storage w = widgets[key];
        w.name = name;
        w.delux = delux;
        w.price = price;
        emit LogNewWidget(msg.sender, key, name, delux, price);
    }
    
    function updateWidget(address key, string memory name, bool delux, uint price) public {
        require(widgetSet.exists(key), "Can't update a widget that doesn't exist.");
        WidgetStruct storage w = widgets[key];
        w.name = name;
        w.delux = delux;
        w.price = price;
        emit LogUpdateWidget(msg.sender, key, name, delux, price);
    }
    
    function remWidget(address key) public {
        widgetSet.remove(key); // Note that this will fail automatically if the key doesn't exist
        delete widgets[key];
        emit LogRemWidget(msg.sender, key);
    }
    
    function getWidget(address key) public view returns(string memory name, bool delux, uint price) {
        require(widgetSet.exists(key), "Can't get a widget that doesn't exist.");
        WidgetStruct storage w = widgets[key];
        return(w.name, w.delux, w.price);
    }
    
    function getWidgetCount() public view returns(uint count) {
        return widgetSet.count();
    }
    
    function getWidgetAtIndex(uint index) public view returns(address key) {
        return widgetSet.keyAtIndex(index);
    }
}
