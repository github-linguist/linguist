import QtQuick 1.0


Item {
    id: top_
    width: 320
    height: 240
    
    property color fg: "red"
    property int size: 42  
    property bool idevice: false
    
    signal stuff();
        
    Rectangle { 
        id: r0_
        anchors.fill: parent 
        color: "blue"
            Behavior on color { ColorAnimation { duration: 1000 } }
        
        Rectangle {
            id: r1_
            anchors.centerIn: parent            
            color: "orange"
            width: 42
            height: 42
            scale: ma_.pressed ? 0.9 : 1
            Behavior on scale { NumberAnimation { duration: 23 } }
            Behavior on color { ColorAnimation { duration: 250 } }
            
            MouseArea {
                id: ma_
                anchors.fill: parent
                onClicked: top_.stuff();
                visible: !top_.idevice
            }
        }            
        
        Text {
            anchors.centerIn: parent
            font.pixelSize: 42
            text: "" + r0_.color + "\n\n" + r1_.color
        }        
    }
    
    onStuff: {
        r0_.color =  Qt.rgba(Math.random(), 0.5, 0, 1); 
        r1_.color =  Qt.rgba(0.5, Math.random(), 0.5, 1); 
    }
}   
