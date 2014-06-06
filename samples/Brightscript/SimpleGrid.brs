' *********************************************************
' **  Simple Grid Screen Demonstration App
' **  Jun 2010
' **  Copyright (c) 2010 Roku Inc. All Rights Reserved.
' *********************************************************

'************************************************************
'** Application startup
'************************************************************
Sub Main()

    'initialize theme attributes like titles, logos and overhang color
    initTheme()
  
    gridstyle = "Flat-Movie"

    'set to go, time to get started
    while gridstyle <> ""
        print "starting grid style= ";gridstyle
        screen=preShowGridScreen(gridstyle)
        gridstyle = showGridScreen(screen, gridstyle)
    end while

End Sub


'*************************************************************
'** Set the configurable theme attributes for the application
'** 
'** Configure the custom overhang and Logo attributes
'** These attributes affect the branding of the application
'** and are artwork, colors and offsets specific to the app
'*************************************************************

Sub initTheme()
    app = CreateObject("roAppManager")
    app.SetTheme(CreateDefaultTheme())
End Sub

'******************************************************
'** @return The default application theme.
'** Screens can make slight adjustments to the default
'** theme by getting it from here and then overriding
'** individual theme attributes.
'******************************************************
Function CreateDefaultTheme() as Object
    theme = CreateObject("roAssociativeArray")

    theme.ThemeType = "generic-dark"

    ' All these are greyscales
    theme.GridScreenBackgroundColor = "#363636"
    theme.GridScreenMessageColor    = "#808080"
    theme.GridScreenRetrievingColor = "#CCCCCC"
    theme.GridScreenListNameColor   = "#FFFFFF"

    ' Color values work here
    theme.GridScreenDescriptionTitleColor    = "#001090"
    theme.GridScreenDescriptionDateColor     = "#FF005B"
    theme.GridScreenDescriptionRuntimeColor  = "#5B005B"
    theme.GridScreenDescriptionSynopsisColor = "#606000"
    
    'used in the Grid Screen
    theme.CounterTextLeft           = "#FF0000"
    theme.CounterSeparator          = "#00FF00"
    theme.CounterTextRight          = "#0000FF"
    
    theme.GridScreenLogoHD          = "pkg:/images/Overhang_Test_HD.png"

    theme.GridScreenLogoOffsetHD_X  = "0"
    theme.GridScreenLogoOffsetHD_Y  = "0"
    theme.GridScreenOverhangHeightHD = "99"

    theme.GridScreenLogoSD          = "pkg:/images/Overhang_Test_SD43.png"
    theme.GridScreenOverhangHeightSD = "66"
    theme.GridScreenLogoOffsetSD_X  = "0"
    theme.GridScreenLogoOffsetSD_Y  = "0"
    
    ' to use your own focus ring artwork 
    'theme.GridScreenFocusBorderSD        = "pkg:/images/GridCenter_Border_Movies_SD43.png"
    'theme.GridScreenBorderOffsetSD  = "(-26,-25)"
    'theme.GridScreenFocusBorderHD        = "pkg:/images/GridCenter_Border_Movies_HD.png"
    'theme.GridScreenBorderOffsetHD  = "(-28,-20)"
    
    ' to use your own description background artwork
    'theme.GridScreenDescriptionImageSD  = "pkg:/images/Grid_Description_Background_SD43.png"
    'theme.GridScreenDescriptionOffsetSD = "(125,170)"
    'theme.GridScreenDescriptionImageHD  = "pkg:/images/Grid_Description_Background_HD.png"
    'theme.GridScreenDescriptionOffsetHD = "(190,255)"
    

    return theme
End Function

'******************************************************
'** Perform any startup/initialization stuff prior to 
'** initially showing the screen.  
'******************************************************
Function preShowGridScreen(style as string) As Object

    m.port=CreateObject("roMessagePort")
    screen = CreateObject("roGridScreen")
    screen.SetMessagePort(m.port)
'    screen.SetDisplayMode("best-fit")
    screen.SetDisplayMode("scale-to-fill")

    screen.SetGridStyle(style)
    return screen

End Function


'******************************************************
'** Display the gird screen and wait for events from 
'** the screen. The screen will show retreiving while
'** we fetch and parse the feeds for the show posters
'******************************************************
Function showGridScreen(screen As Object, gridstyle as string) As string

    print "enter showGridScreen"

    categoryList = getCategoryList()
    categoryList[0] = "GridStyle: " + gridstyle
    screen.setupLists(categoryList.count())
    screen.SetListNames(categoryList)
    StyleButtons = getGridControlButtons()
    screen.SetContentList(0, StyleButtons)
    for i = 1 to categoryList.count()-1
        screen.SetContentList(i, getShowsForCategoryItem(categoryList[i]))
    end for
    screen.Show()

    while true
        print "Waiting for message"
        msg = wait(0, m.port)
        'msg = wait(0, screen.GetMessagePort())     ' getmessageport does not work on gridscreen
        print "Got Message:";type(msg)
        if type(msg) = "roGridScreenEvent" then
            print "msg= "; msg.GetMessage() " , index= "; msg.GetIndex(); " data= "; msg.getData()
            if msg.isListItemFocused() then
                print"list item focused | current show = "; msg.GetIndex()
            else if msg.isListItemSelected() then
                row = msg.GetIndex()
                selection = msg.getData()
                print "list item selected row= "; row; " selection= "; selection

                ' Did we get a selection from the gridstyle selection row?
                if (row = 0)
                    ' yes, return so we can come back with new style
                    return StyleButtons[selection].Title
                endif

                'm.curShow = displayShowDetailScreen(showList[msg.GetIndex()])
            else if msg.isScreenClosed() then
                return ""
            end if
        end If
    end while


End Function

'**********************************************************
'** When a poster on the home screen is selected, we call
'** this function passing an roAssociativeArray with the 
'** ContentMetaData for the selected show.  This data should 
'** be sufficient for the springboard to display
'**********************************************************
Function displayShowDetailScreen(category as Object, showIndex as Integer) As Integer

    'add code to create springboard, for now we do nothing
    return 1

End Function


'**************************************************************
'** Return the list of categories to display in the filter
'** banner. The result is an roArray containing the names of 
'** all of the categories. All just static data for the example.
'***************************************************************
Function getCategoryList() As Object

    categoryList = [ "GridStyle", "Reality", "History", "News", "Comedy", "Drama"]
    return categoryList

End Function


'********************************************************************
'** Given the category from the filter banner, return an array 
'** of ContentMetaData objects (roAssociativeArray's) representing 
'** the shows for the category. For this example, we just cheat and
'** create and return a static array with just the minimal items
'** set, but ideally, you'd go to a feed service, fetch and parse
'** this data dynamically, so content for each category is dynamic
'********************************************************************
Function getShowsForCategoryItem(category As Object) As Object

    print "getting shows for category "; category

    showList = [
        {
            Title: category + ": Header",
            releaseDate: "1976",
            length: 3600-600,
            Description:"This row is category " + category,
            hdBranded: true,
            HDPosterUrl:"http://upload.wikimedia.org/wikipedia/commons/4/43/Gold_star_on_blue.gif",
            SDPosterUrl:"http://upload.wikimedia.org/wikipedia/commons/4/43/Gold_star_on_blue.gif",
            Description:"Short Synopsis #1",
            Synopsis:"Length",
            StarRating:10,
        }
        {
            Title: category + ": Beverly Hillbillies",
            releaseDate: "1969",
            rating: "PG",
            Description:"Come and listen to a story about a man named Jed: Poor mountaineer, barely kept his family fed. Then one day he was shootin at some food, and up through the ground came a bubblin crude. Oil that is, black gold, Texas tea.",
            numEpisodes:42,
            contentType:"season",
            HDPosterUrl:"http://upload.wikimedia.org/wikipedia/en/4/4e/The_Beverly_Hillbillies.jpg",
            SDPosterUrl:"http://upload.wikimedia.org/wikipedia/en/4/4e/The_Beverly_Hillbillies.jpg",
            StarRating:80,
            UserStarRating:40
        }
        {
            Title: category + ": Babylon 5",
            releaseDate: "1996",
            rating: "PG",
            Description:"The show centers on the Babylon 5 space station: a focal point for politics, diplomacy, and conflict during the years 2257-2262.",
            numEpisodes:102,
            contentType:"season",
            HDPosterUrl:"http://upload.wikimedia.org/wikipedia/en/9/9d/Smb5-s4.jpg",
            SDPosterUrl:"http://upload.wikimedia.org/wikipedia/en/9/9d/Smb5-s4.jpg",
            StarRating:80,
            UserStarRating:40
        }
        {
            Title: category + ": John F. Kennedy",
            releaseDate: "1961",
            rating: "PG",
            Description:"My fellow citizens of the world: ask not what America will do for you, but what together we can do for the freedom of man.",
            HDPosterUrl:"http://upload.wikimedia.org/wikipedia/en/5/52/Jfk_happy_birthday_1.jpg",
            SDPosterUrl:"http://upload.wikimedia.org/wikipedia/en/5/52/Jfk_happy_birthday_1.jpg",
            StarRating:100
        }
        {
            Title: category + ": Man on the Moon",
            releaseDate: "1969",
            rating: "PG",
            Description:"That's one small step for a man, one giant leap for mankind.",
            HDPosterUrl:"http://upload.wikimedia.org/wikipedia/commons/1/1e/Apollo_11_first_step.jpg",
            SDPosterUrl:"http://upload.wikimedia.org/wikipedia/commons/1/1e/Apollo_11_first_step.jpg",
            StarRating:100
        }
        {
            Title: category + ": I have a Dream",
            releaseDate: "1963",
            rating: "PG",
            Description:"I have a dream that my four little children will one day live in a nation where they will not be judged by the color of their skin, but by the content of their character.",
            HDPosterUrl:"http://upload.wikimedia.org/wikipedia/commons/8/81/Martin_Luther_King_-_March_on_Washington.jpg",
            SDPosterUrl:"http://upload.wikimedia.org/wikipedia/commons/8/81/Martin_Luther_King_-_March_on_Washington.jpg",
            StarRating:100
        }
    ]

    return showList
End Function
    
function getGridControlButtons() as object
        buttons = [
            { Title: "Flat-Movie"
              ReleaseDate: "HD:5x2 SD:5x2"
              Description: "Flat-Movie (Netflix) style"
              HDPosterUrl:"http://upload.wikimedia.org/wikipedia/commons/4/43/Gold_star_on_blue.gif"
              SDPosterUrl:"http://upload.wikimedia.org/wikipedia/commons/4/43/Gold_star_on_blue.gif"
            }
            { Title: "Flat-Landscape"
              ReleaseDate: "HD:5x3 SD:4x3"
              Description: "Channel Store"
              HDPosterUrl:"http://upload.wikimedia.org/wikipedia/commons/thumb/9/96/Dunkery_Hill.jpg/800px-Dunkery_Hill.jpg",
              SDPosterUrl:"http://upload.wikimedia.org/wikipedia/commons/thumb/9/96/Dunkery_Hill.jpg/800px-Dunkery_Hill.jpg",
            }
            { Title: "Flat-Portrait"
              ReleaseDate: "HD:5x2 SD:5x2"
              Description: "3x4 style posters"
              HDPosterUrl:"http://upload.wikimedia.org/wikipedia/commons/9/9f/Kane_George_Gurnett.jpg",
              SDPosterUrl:"http://upload.wikimedia.org/wikipedia/commons/9/9f/Kane_George_Gurnett.jpg",
            }
            { Title: "Flat-Square"
              ReleaseDate: "HD:7x3 SD:6x3"
              Description: "1x1 style posters"
              HDPosterUrl:"http://upload.wikimedia.org/wikipedia/commons/thumb/d/de/SQUARE_SHAPE.svg/536px-SQUARE_SHAPE.svg.png",
              SDPosterUrl:"http://upload.wikimedia.org/wikipedia/commons/thumb/d/de/SQUARE_SHAPE.svg/536px-SQUARE_SHAPE.svg.png",
            }
            { Title: "Flat-16x9"
              ReleaseDate: "HD:5x3 SD:4x3"
              Description: "HD style posters"
              HDPosterUrl:"http://upload.wikimedia.org/wikipedia/commons/thumb/2/22/%C3%89cran_TV_plat.svg/200px-%C3%89cran_TV_plat.svg.png",
              SDPosterUrl:"http://upload.wikimedia.org/wikipedia/commons/thumb/2/22/%C3%89cran_TV_plat.svg/200px-%C3%89cran_TV_plat.svg.png",
            }
       ]
       return buttons
End Function
