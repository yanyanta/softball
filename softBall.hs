{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import Graphics.UI.SDL as SDL
import Data.List
import Data.Word
import Data.Maybe
import qualified Data.Map as M

-- | Screen informations
cellSide = 36
xCellNum = 6
yCellNum = 13
scrPad = 5
scrWidth = cellSide*xCellNum+2*scrPad
scrHeight = cellSide*yCellNum+2*scrPad

-- | Data declaration part
-- | The 'State' data structure contains game state for eventLoop.
data State = State {ctrlBalls :: CtrlBalls,
                    heapBalls :: (HeapBalls),
                    waitingBalls :: [CtrlBalls],
                    outline :: Outline,
                    frame :: Frame,
                    flag :: StateFlag,
                    screen :: SDL.Surface,
                    images :: Images}
             
type HeapBalls = M.Map Coor Ball

data CtrlBalls = CtrlBalls {balls::Balls, angle::Angle}
               deriving Show

data Ball = Ball {coor :: Coor, kind  :: Kind}
      deriving (Show,Eq)

data Images = Images {red  :: SDL.Surface,
                      blue :: SDL.Surface,
                      green:: SDL.Surface,
                      yellow :: SDL.Surface}

data Kind = Red | Blue | Green | Yellow
          deriving (Show,Eq, Enum, Bounded, Ord)
 
data ArrowKey = LeftKey | DownKey | RightKey | UpKey
              deriving (Eq, Enum, Bounded)

data Angle = Downward | Leftward | Upward | Rightward
           deriving (Show, Eq, Enum, Bounded)

data StateFlag = Control | Clear | Descent | End
               deriving (Show,Eq)

-- | Class declaration part
class Obj a where
  pos :: a -> Coor
  x :: a -> Int
  x = fst . pos
  y :: a -> Int
  y = snd . pos
  getImage :: Images -> a -> SDL.Surface

class Drawable a where
  draw :: State -> a -> IO Bool

class Movable a where
  move :: a -> Coor -> Outline -> Maybe a
  moveInField :: a -> Coor -> Outline -> a
  moveInField z coor outline =
    case move z coor outline of
      Just z' -> z'
      Nothing -> z

class Circular a where
  csucc :: (Eq a, Enum a, Bounded a) => a -> a
  cpred :: (Eq a, Enum a, Bounded a) => a -> a
  csucc x | x == maxBound = minBound
          | otherwise     = succ x
  cpred x | x == minBound = maxBound
          | otherwise     = pred x
                        
-- | Instance declaration
instance Obj Ball where
  pos = coor
  getImage images ball =
         case kind ball of
           Red    -> red images
           Blue   -> blue images
           Green  -> green images
           Yellow -> yellow images

instance Drawable CtrlBalls where
  draw state (CtrlBalls {balls,..}) = mapM_ (draw state) balls >> return True

instance Drawable a => Drawable (M.Map k a) where
  draw scr heap = mapM_ (draw scr) (M.elems heap) >> return True

instance Drawable Ball where
  draw state ball = SDL.blitSurface 
                    (getImage (images state) ball)
                    (Just $ SDL.Rect 0 0 cellSide cellSide)
                    (screen state)
                    (Just $ SDL.Rect ((x ball)*cellSide+scrPad)
                     (scrHeight-(((y ball)+1)*cellSide+scrPad))
                     cellSide cellSide)

instance Movable Ball where
    move ball delta outline =
      let newCoor = coor ball |+| delta in
      if movableCoor newCoor outline
      then Just (ball {coor=newCoor}) 
      else Nothing

instance Movable CtrlBalls where
  move (CtrlBalls {balls, angle}) delta outline = do
    movedBalls <- allJust $ map (\ball-> move ball delta outline) balls 
    return (CtrlBalls {balls = movedBalls, angle})
  
instance Show State where
  show state = show (ctrlBalls state) ++ " " ++ show (heapBalls state) ++ " " ++ show (outline state)

instance Circular Angle

-- | Type declarations
type Balls = [Ball]
type Outline = [Int] -- 0~5列目までの山の高さ
type Coor = (Int, Int)
type Time = Word32
type Frame = Word

-- | Erasetion declarations
(|+|) :: Coor -> Coor -> Coor
(x1,y1) |+| (x2,y2) = (x1+x2, y1+y2)

inField :: Coor -> Bool
inField (x, y)
  | 0 <= x && x <= 5 && 0 <= y = True
  | otherwise = False

movableCoor :: Coor -> Outline -> Bool
movableCoor (x, y) outline
  | inField (x, y) && y >= outline !! x = True
  | otherwise = False
  
rotate :: [Coor] -> CtrlBalls -> Outline -> CtrlBalls
rotate dels CtrlBalls {balls = [ker, ano], angle = ang} outline =
  let nextAno = ano {coor = coor ano |+| (dels !! fromEnum ang)}
      nextCtrl = CtrlBalls {balls = [ker, nextAno], angle = csucc ang}
  in
   if movableCoor (coor nextAno) outline
   then nextCtrl
   else rotate dels nextCtrl outline
rotateR :: CtrlBalls -> Outline -> CtrlBalls
rotateR = rotate [(-1, 1), (1, 1), (1, -1), (-1, -1)]  
rotateL :: CtrlBalls -> Outline -> CtrlBalls
rotateL = rotate [(1, 1), (-1, 1), (-1, -1), (1, -1)]
                
ctrlGen :: [CtrlBalls] -> (CtrlBalls, [CtrlBalls])
ctrlGen lst =
  case splitAt 1 lst of
    ([newCtrl],rest) -> (newCtrl, rest)
    _ -> error "ERROR at ctrlGen"

getHeapFirst :: HeapBalls -> Maybe Ball
getHeapFirst heap
  | M.null heap = Nothing
  | otherwise   = Just . snd $ M.elemAt 0 heap

insertBallToHeap :: Ball -> HeapBalls -> HeapBalls
insertBallToHeap ball = M.insert (coor ball) ball

insertBallsToHeap :: Balls -> HeapBalls -> HeapBalls
insertBallsToHeap bs heap =
  foldr (\ball -> insertBallToHeap ball) heap bs

allJust :: [Maybe a] -> Maybe [a]
allJust lst =
  if all isJust lst then Just $ map fromJust lst else Nothing

gameinit :: IO State
gameinit = do
       SDL.init [SDL.InitEverything]
       scr <- SDL.setVideoMode scrWidth scrHeight 32 []
       SDL.setCaption "Soft Ball" "Soft Ball"
       let waitings = initCtrlBalls (cycle [Red,Blue,Green,Yellow,Red,Green,Blue,Yellow,Blue])
           (initialCtrlBalls, initialWaitingBalls) = ctrlGen waitings
       redBallImage <- SDL.loadBMP "./red.bmp"
       blueBallImage <- SDL.loadBMP "./blue.bmp"
       greenBallImage <- SDL.loadBMP "./green.bmp"
       yellowBallImage <- SDL.loadBMP "./yellow.bmp"
       return State {ctrlBalls = initialCtrlBalls,
                     heapBalls = M.empty,
                     waitingBalls = initialWaitingBalls,
                     frame = 0,
                     outline = [0,0,0,0,0,0],
                     flag = Control,
                     screen = scr,
                     images = Images {red = redBallImage,
                                      blue = blueBallImage,
                                      green = greenBallImage,
                                      yellow = yellowBallImage
                                     }
                    }

gameend :: IO ()
gameend = SDL.quit >> print "Over"

wait :: Time -> IO Time
wait span = do
  start <- SDL.getTicks
  SDL.delay span
  return (start+span)
      
initCtrlBalls :: [Kind] -> [CtrlBalls]
initCtrlBalls (l:r:t) =
  CtrlBalls { balls = [Ball {coor = (3, 12), kind = l},
                       Ball {coor = (3, 13), kind = r}],
              angle = Upward
            } : initCtrlBalls t
initCtrlBalls _ = error "INIT_CTRLBALL"

descent :: State -> Maybe State
descent state = do
  movedCtrlBalls <- move (ctrlBalls state) (0,-1) (outline state)
  return state {ctrlBalls = movedCtrlBalls}

updateOutline :: Coor -> Outline -> Outline
updateOutline (0, y) (_:hs) = y:hs
updateOutline (x, y) (h:hs) = h : updateOutline (x-1, y) hs

createOutline :: HeapBalls -> Outline
createOutline heap = map length (toXcolList heap)

descentTillLand :: Outline -> Ball -> (Ball, Outline)
descentTillLand outline ball =
  let newCoor@(x,y) = (fst (coor ball),outline !! fst (coor ball)) in
  (ball {coor = newCoor}, updateOutline (x,y+1) outline)

freeFall :: State -> State
freeFall state@(State {ctrlBalls, outline, waitingBalls, ..}) =
  state { ctrlBalls = newCtrl, heapBalls = newHeap,
          waitingBalls = newWaiting, outline = createOutline newHeap}
  where
    CtrlBalls {balls = balls@[kernel, another], angle} = ctrlBalls
    (newCtrl, newWaiting) = ctrlGen waitingBalls
    newHeap =
      case angle of
        Downward ->
          let (ano, ol) = descentTillLand outline another 
              (ker,  _) = descentTillLand ol      kernel
          in insertBallsToHeap [ano,ker] heapBalls
        Upward   ->
          let (ker, ol) = descentTillLand outline kernel
              (ano,  _) = descentTillLand ol      another
          in insertBallsToHeap [ano,ker] heapBalls
        _ -> insertBallsToHeap
             (map (fst.descentTillLand outline) balls) heapBalls

toXcolList :: M.Map Coor Ball -> [Balls]
toXcolList heap =
  helper 0 $ groupBy (\b1 b2 -> fst (coor b1) == fst (coor b2)) (M.elems heap)
  where
    helper :: Int -> [Balls] -> [Balls]
    helper n balls
      | n == xCellNum = []
      | otherwise = case balls of
        lst@(hd@(h:_):t) ->
          if fst (coor h) == n then hd : helper (n+1) t
          else [] : helper (n+1) lst
        [] ->  [] : helper (n+1) []
        _ -> error "toXcolList"

normalize :: HeapBalls -> HeapBalls
normalize heap =
  M.fromList $ 
  concatMap (\col -> zipWith (\n b-> let c = (x b, n)
                                     in (c,b{coor = c}))
                     [0, 1 .. (length col -1)] col)
  (toXcolList heap)

getHeap :: Coor -> HeapBalls -> Maybe Ball
getHeap = M.lookup

nextToBall ::  HeapBalls -> Ball -> Balls
nextToBall heap ball =
  [nextBall|d <- [(0,-1),(-1,0),(0,1),(1,0)],
   let
     nextCoor = d |+| cor
     nextBallMaybe = getHeap nextCoor heap,
   isJust nextBallMaybe,
   let nextBall = fromJust nextBallMaybe,
   knd == kind nextBall]
  where cor = coor ball
        knd = kind ball

delBall :: Ball -> HeapBalls -> HeapBalls
delBall ball heap = M.delete (coor ball) heap

delBalls :: Balls -> HeapBalls -> HeapBalls
delBalls balls heap = foldr delBall heap balls

erase :: HeapBalls -> Ball -> Balls
erase heap start =
  let nexts = nextToBall heap start in
  start : concatMap (erase (delBalls nexts heap)) nexts

eraseAll :: HeapBalls -> [Balls]
eraseAll heap =
  case getHeapFirst heap of
    Nothing -> []
    Just h ->
      hConnection : eraseAll (delBalls hConnection heap)
      where
        hConnection = erase (delBall h heap) h

clear :: State -> Maybe State
clear state@(State {heapBalls = heap, ..}) =
  if null ballsTodisppear then Nothing else Just newState
  where
    connectedList = eraseAll heap
    ballsTodisppear = concat . filter ((4<=) . length) $ connectedList
    normalizedHeap = normalize $ delBalls ballsTodisppear heap
    newState = state {heapBalls = normalizedHeap,
                      outline = createOutline normalizedHeap
                     }

defaultUpdate :: State -> State
defaultUpdate state = state {frame = newFrame, flag = newFlag}
  where
    newFrame = frame state + 1
    newFlag = case flag state of
      Control
        | newFrame `mod` 10 == 0 -> Descent
        | otherwise -> Control
      x -> x

overp :: State -> Bool
overp state = 13<(outline state)!!3

drawState :: State -> IO ()
drawState state@(State {ctrlBalls, heapBalls,screen,..}) = do
  black <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 0 0
  SDL.fillRect screen (Just $ SDL.Rect 0 0 scrWidth scrHeight) black
  draw state ctrlBalls
  draw state heapBalls
  SDL.flip screen
 
main :: IO ()
main = gameinit >>= mainLoop

mainLoop :: State -> IO ()
mainLoop state = case flag state of
  End -> gameend
  Control -> checkEvent state >>= drawWaitUpdateLoop 60
  Descent -> case descent state of
    Just state -> drawWaitUpdateLoop 60 state {flag = Control}
    Nothing    -> drawWaitUpdateLoop 60 $ freeFall state {flag = Clear}
  Clear -> case clear state of
    Just clearedState -> drawWaitUpdateLoop 400 clearedState
    Nothing
      | overp state -> drawWaitUpdateLoop 0 state {flag = End}
      | otherwise -> drawWaitUpdateLoop 60 state{flag = Control}
  where
    drawWaitUpdateLoop time state =
      drawState state >> wait time >> mainLoop (defaultUpdate state)
  
checkEvent :: State -> IO State
checkEvent state = do
  event <- SDL.pollEvent
  return $
    case event of
      KeyUp (Keysym key _ _) ->
        case key of
          SDLK_ESCAPE -> state {flag = End}
          SDLK_UP -> state {ctrlBalls = rotateR (ctrlBalls state) (outline state)}
          SDLK_LEFT -> state {ctrlBalls = moveInField (ctrlBalls state) (-1, 0) (outline state)}
          SDLK_RIGHT -> state {ctrlBalls = moveInField (ctrlBalls state) (1, 0) (outline state)}
          SDLK_DOWN -> freeFall state {flag = Clear}
          _ -> state
      _ -> state