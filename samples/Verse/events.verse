using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/UI }

(Event:event(t) where t:type).Call<public>(Function:type{_(:t):void})<suspends>:void=
    loop:
        EventData := Event.Await()
        Function(EventData)


wrapper_agent(t : type) := class():
    ExtraData : t
    OutputFunc : tuple(agent, t) -> void
    InputFunc(Agent : agent): void = OutputFunc(Agent, ExtraData)



(Listenable : listenable(agent)).SubscribeAgent<public>(OutputFunc : tuple(agent, t)->void, ExtraData : t where t:type) : cancelable =
    Wrapper := wrapper_agent(t){ExtraData := ExtraData, OutputFunc := OutputFunc}
    Listenable.Subscribe(Wrapper.InputFunc)



#Wrapper widget_message
wrapper_widget(t : type) := class():
    ExtraData : t
    OutputFunc : tuple(widget_message, t) -> void
    InputFunc(Widget : widget_message): void = OutputFunc(Widget, ExtraData)


(Listenable : listenable(widget_message)).SubscribeWidget<public>(OutputFunc : tuple(widget_message, t)->void, ExtraData : t where t:type) : cancelable =
    Wrapper := wrapper_widget(t){ExtraData := ExtraData, OutputFunc := OutputFunc}
    Listenable.Subscribe(Wrapper.InputFunc)


    
wrapper(t : type ) := class():
    ExtraData : t
    OutputFunc : t -> void
    InputFunc(): void = OutputFunc(ExtraData)

