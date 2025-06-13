using. /Verse.org/Simulation
using. /Fortnite.com/Devices

using. Vertex

selector_device_v0_1<public> := class(tableau_device):
    DeviceType<override>:string = "SELECTOR"

    @editable Items<public>:strs_plug = strs_plug{}
    @editable Wrap<public>:bool_plug = bool_plug. Default := true
    @editable Index<public>:int_plug = int_plug. Default := 1

    GetResources<override>():[]resource=
        CreatedProps := CombineAll of array:
            CreateMap of for(I->Item:Items.Default). ("{Item} Index", Integer of I+1)
            CreateMap of for(Item:Items.Default). ("{Item} Label", Str of Item)
            CreateMap of for(Item:Items.Default). ("Is {Item}", Bool of (Path("Current Index"), "Int =", Path("{Item} Index")))

        CreatedActions := CombineAll of array:
            CreateMap of for(Item:Items.Default). ("Set {Item}", Action of Set("Current Index").To("{Item} Index"))

        array. resource:
            Props := CreatedProps.CombineWithAll of map:
                "All Types" => Strs of Items
                "Selection" => Str of (Path("All Types"), "Get", Path("Current Index"))
                "Current Index" => Integer of Index
                "New Index" => Integer of (Path("Current Index"), "+", ONE())
                "Previous Index" => Integer of (Path("Current Index"), "-", ONE())
                "Type Count" => Integer of (Path("All Types"), "Length")
                "Is Last Type" => Bool of (Path("Current Index"), "Int >=", Path("Type Count"))
                "Is First Type" => Bool of (Path("Current Index"), "Int <=", ONE())
                "Wrap" => Bool of Wrap
            Actions := CreatedActions.CombineWithAll of map:
                "Next" => Action of array:
                    IsTrue("Is Last Type").Else("Go to Next")
                    IsTrue("Wrap")
                    Set("Current Index").To(ONE())
                "Previous" => Action of array:
                    IsTrue("Is First Type").Else("Go to Previous")
                    IsTrue("Wrap")
                    Set("Current Index").To("Type Count")
                "Go to Next" => Action of Set("Current Index").To("New Index")
                "Go to Previous" => Action of Set("Current Index").To("Previous Index")
                "Insert" => Action of Insert(TWEAK()).Into("All Types")
                "Remove" => Action of RemoveItem(TWEAK()).From("All Types")