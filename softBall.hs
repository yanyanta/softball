{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Graphics.UI.SDL as SDL
import Data.List as L
import Data.Word
import Data.Map as M
import Data.Maybe
import Control.Lens
import Debug.Trace

-- | Screen informations
cellSide = 36
xCellNum = 6
yCellNum = 13
scrPad = 5
scrWidth = cellSide*xCellNum+2*scrPad
scrHeight = cellSide*yCellNum+2*scrPad

-- | Data declaration part
-- | The 'State' data structure contains game state for eventLoop.
data State = State {_ctrlBalls :: Ctrl,
                    _heapBalls :: Heap,
                    _waitingBalls :: [Ctrl],
                    _outline :: Outline,
                    _frame :: Frame,
                    _flag :: StateFlag,
                    _screen :: SDL.Surface,
                    _images :: Images}
             
data Ctrl = Ctrl {_balls::Balls, _angle::Angle}
               deriving Show

data Ball = Ball {_coor :: Coor, _kind  :: Kind}
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

-- | Type declarations
type Balls = [Ball]
type Ballss = [Balls]
type Heap = M.Map Coor Kind
type Outline = [Int] -- 0~5列目までの山の高さ
type Coor = (Int, Int)
type Time = Word32
type Frame = Word

-- | Class declaration part
class Obj a where
  pos :: a -> Coor

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
                        
-- | Making lenses
$(makeLenses ''State)
$(makeLenses ''Ball)
$(makeLenses ''Ctrl)
x = _1
y = _2

-- | Instance declaration
instance Obj Ball where
  pos = view coor

instance Drawable Ctrl where
  draw state ctrl = mapM_ (draw state) (view balls ctrl) >> return True

instance Drawable Ball where
  draw state ball = SDL.blitSurface 
                    (kindToImage (view images state) (view kind ball))
                    (Just $ SDL.Rect 0 0 cellSide cellSide)
                    (view screen state)
                    (Just $ SDL.Rect ((view (coor.x) ball)*cellSide+scrPad)
                     (scrHeight-(((view (coor.y) ball)+1)*cellSide+scrPad))
                     cellSide cellSide)

instance Drawable Heap where
  draw scr heap = mapM_ (\(c,k) -> draw scr (Ball {_coor=c, _kind = k}))
                  (M.toList heap) >> return True

instance Movable Ball where
    move ball delta outline =
      let newCoor = view coor ball |+| delta in
      if movableCoor newCoor outline
      then Just (set coor newCoor ball) 
      else Nothing

instance Movable Ctrl where
  move ctrl delta outline = do
    movedBalls <- allJust $ Prelude.map (\ball-> move ball delta outline)
                  (view balls ctrl)
    return (set balls movedBalls ctrl)
  
instance Show State where
  show state = show (view ctrlBalls state) ++ " " ++ show (view heapBalls state) ++ " " ++ show (view outline state)

instance Circular Angle

-- | Function declarations
kindToImage :: Images -> Kind ->SDL.Surface
kindToImage images kind = case kind of
  Red    -> red images
  Blue   -> blue images
  Green  -> green images
  Yellow -> yellow images

(|+|) :: Coor -> Coor -> Coor
(x1,y1) |+| (x2,y2) = (x1+x2, y1+y2)

inField :: Coor -> Bool
inField (x, y)
  | 0 <= x && x < xCellNum && 0 <= y = True
  | otherwise = False

movableCoor :: Coor -> Outline -> Bool
movableCoor (x, y) outline
  | inField (x, y) && y >= outline !! x = True
  | otherwise = False
  
rotate :: [Coor] -> Ctrl -> Outline -> Ctrl
rotate dels Ctrl {_balls = [ker, ano], _angle = ang} outline =
  let nextAno = over coor (|+| (dels !! fromEnum ang)) ano
      nextCtrl = Ctrl {_balls = [ker, nextAno], _angle = csucc ang}
  in
   if movableCoor (view coor nextAno) outline
   then nextCtrl
   else rotate dels nextCtrl outline

rotateR :: Ctrl -> Outline -> Ctrl
rotateR = rotate [(-1, 1), (1, 1), (1, -1), (-1, -1)]  
rotateL :: Ctrl -> Outline -> Ctrl
rotateL = rotate [(1, 1), (-1, 1), (-1, -1), (1, -1)]
                
ctrlGen :: [Ctrl] -> (Ctrl, [Ctrl])
ctrlGen lst =
  case splitAt 1 lst of
    ([newCtrl],rest) -> (newCtrl, rest)
    _ -> error "ERROR at ctrlGen"

elems :: [[a]] -> [a]
elems = concat

getHeapFirst :: Heap -> Maybe (Coor, Kind)
getHeapFirst heap
  | M.null heap = Nothing
  | otherwise   = Just $ M.elemAt 0 heap

insertBallToHeap :: Ball -> Heap -> Heap
insertBallToHeap ball = M.insert (view coor ball) (view kind ball)

insertBallsToHeap :: Balls -> Heap -> Heap
insertBallsToHeap bs heap =
  Prelude.foldr (\ball -> insertBallToHeap ball) heap bs

allJust :: [Maybe a] -> Maybe [a]
allJust lst =
  if all isJust lst then Just $ Prelude.map fromJust lst else Nothing

gameinit :: IO State
gameinit = do
       SDL.init [SDL.InitEverything]
       scr <- SDL.setVideoMode scrWidth scrHeight 32 []
       SDL.setCaption "Soft Ball" "Soft Ball"
       let waitings = initCtrl (cycle [Red,Blue,Green,Yellow,Red,Green,Blue,Yellow,Blue])
           (initialCtrl, initialWaitingBalls) = ctrlGen waitings
       redBallImage <- SDL.loadBMP "./red.bmp"
       blueBallImage <- SDL.loadBMP "./blue.bmp"
       greenBallImage <- SDL.loadBMP "./green.bmp"
       yellowBallImage <- SDL.loadBMP "./yellow.bmp"
       return State {_ctrlBalls = initialCtrl,
                     _heapBalls = M.empty,
                     _waitingBalls = initialWaitingBalls,
                     _frame = 0,
                     _outline = [0,0,0,0,0,0],
                     _flag = Control,
                     _screen = scr,
                     _images = Images {red = redBallImage,
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
      
initCtrl :: [Kind] -> [Ctrl]
initCtrl (l:r:t) =
  Ctrl { _balls = [Ball {_coor = (3, 12), _kind = l},
                       Ball {_coor = (3, 13), _kind = r}],
         _angle = Upward
       } : initCtrl t

initCtrl _ = error "INIT_CTRLBALL"

descent :: State -> Maybe State
descent state = do
  movedCtrl <- move (view ctrlBalls state) (0,-1) (view outline state)
  return $ set ctrlBalls movedCtrl state

updateOutline :: Coor -> Outline -> Outline
updateOutline (0, y) (_:hs) = y:hs
updateOutline (x, y) (h:hs) = h : updateOutline (x-1, y) hs

toXcolList :: M.Map Coor Kind -> [[Coor]]
toXcolList heap =
  helper 0 $ groupBy (\c1 c2 -> view x c1 == view x c2) (M.keys heap)
  where
    helper :: Int -> [[Coor]] -> [[Coor]]
    helper n css
      | n == xCellNum = []
      | otherwise = case css of
        ((c:t):cs) ->
          if view x c == n then (c:t) : helper (n+1) cs
          else [] : helper (n+1) css
        [] ->  [] : helper (n+1) []
        _ -> error "toXcolList"

createOutline :: Heap -> Outline
createOutline heap = Prelude.map length (toXcolList heap)

descentTillLand :: Outline -> Ball -> (Ball, Outline)
descentTillLand outline ball =
  let newCoor@(x,y) = (fst (pos ball),outline !! fst (pos ball)) in
  (set coor newCoor ball, updateOutline (x,y+1) outline)

freeFall :: State -> State
freeFall state =
  state { _ctrlBalls = newCtrl, _heapBalls = newHeap,
          _waitingBalls = newWaiting, _outline = newOutline}
  where
    (newCtrl, newWaiting) = ctrlGen $ view waitingBalls state
    (newHeap, newOutline) =
      L.foldr (\b (h, o) -> let (nb, no) = descentTillLand o b
                          in (insertBallToHeap nb h, no))
      (view heapBalls state, view outline state)
      (sortBy (\b1 b2 -> view (coor.y) b2 `compare` view (coor.y) b1)
       (view balls (view ctrlBalls state)))

normalize :: Heap -> Heap
normalize heap =
  M.fromList $ 
  concatMap (\col -> zipWith (\n c -> let newC = (view x c, n)
                                      in (newC, heap M.! c)) -- kokojanai
                     [0, 1 .. (length col -1)] col)
  (toXcolList heap)
  
getHeap :: Coor -> Heap -> Maybe Kind
getHeap = M.lookup

nextToBall ::  Heap -> Coor -> Kind -> [Coor]
nextToBall heap coor knd =
  [nextCoor|d <- [(0,-1),(-1,0),(0,1),(1,0)],
   let
     nextCoor = d |+| coor
     nextKind = getHeap nextCoor heap,
   isJust nextKind,
   knd ==  fromJust nextKind]

delBall :: Coor -> Heap -> Heap
delBall coor heap = M.delete coor heap

delBalls :: [Coor] -> Heap -> Heap
delBalls balls heap = L.foldr delBall heap balls

erase :: Heap -> Coor -> Kind -> [Coor]
erase heap start kind =
  let nexts = nextToBall heap start kind in
  start : concatMap (\c -> erase (delBalls nexts heap) c kind) nexts

eraseAll :: Heap -> [[Coor]]
eraseAll heap =
  case getHeapFirst heap of
    Nothing     -> []
    Just (c, k) ->
      hConnection : eraseAll (delBalls hConnection heap)
      where
        hConnection = erase (delBall c heap) c k

clear :: State -> Maybe State
clear state =
  if L.null ballsTodisppear then Nothing else Just newState
  where
    connectedList = eraseAll (view heapBalls state)
    ballsTodisppear = concat . L.filter ((4<=) . length) $ connectedList
    prepare = delBalls ballsTodisppear (view heapBalls state)
    normalizedHeap = normalize $ prepare 
    newState = state {_heapBalls = normalizedHeap,
                      _outline = createOutline normalizedHeap
                     }

defaultUpdate :: State -> State
defaultUpdate state = state {_frame = newFrame, _flag = newFlag}
  where
    newFrame = view frame state + 1
    newFlag = case view flag state of
      Control
        | newFrame `mod` 10 == 0 -> Descent
        | otherwise -> Control
      x -> x

overp :: State -> Bool
overp state = 13< view outline state !! 3

drawState :: State -> IO ()
drawState state = do
  black <- SDL.mapRGB (SDL.surfaceGetPixelFormat (view screen state)) 0 0 0
  SDL.fillRect (view screen state) (Just $ SDL.Rect 0 0 scrWidth scrHeight) black
  draw state (view ctrlBalls state)
  draw state (view heapBalls state)
  SDL.flip (view screen state)
 
main :: IO ()
main = gameinit >>= mainLoop

mainLoop :: State -> IO ()
mainLoop state = case view flag state of
  End -> gameend
  Control -> checkEvent state >>= drawWaitUpdateLoop 60
  Descent -> case descent state of
    Just state -> drawWaitUpdateLoop 60 $ set flag Control state
    Nothing    -> drawWaitUpdateLoop 60 . freeFall $ set flag  Clear state 
  Clear -> case clear state of
    Just clearedState -> drawWaitUpdateLoop 400 clearedState
    Nothing
      | overp state -> drawWaitUpdateLoop 0 $ set flag End state
      | otherwise -> drawWaitUpdateLoop 60 $ set flag Control state
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
          SDLK_ESCAPE -> set flag End state
          SDLK_UP -> set ctrlBalls  (rotateR (view ctrlBalls state) (view outline state)) state
          SDLK_LEFT -> set ctrlBalls (moveInField (view ctrlBalls state) (-1, 0) (view outline state)) state
          SDLK_RIGHT -> set ctrlBalls (moveInField (view ctrlBalls state) (1, 0) (view outline state)) state
          SDLK_DOWN -> freeFall $ set flag Clear state
          _ -> state
      _ -> state