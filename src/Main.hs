import Charts

main :: IO ()
main = do
  renderableToPNGFile (toRenderable charts) 640 480 "kalman.png"
  renderableToWindow (toRenderable charts) 640 480
