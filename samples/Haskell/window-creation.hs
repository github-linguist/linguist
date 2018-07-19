import Graphics.HGL

aWindow =  runGraphics $
  withWindow_ "Rosetta Code task: Creating a window" (300, 200) $ \ w -> do
	drawInWindow w $ text (100, 100) "Hello World"
	getKey w
