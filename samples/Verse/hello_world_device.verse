using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }

hello_world_device := class(creative_device):
    @editable
    var Message : string = "Hello, World!"
    
    OnBegin<override>()<suspends>:void =
        Print(Message)