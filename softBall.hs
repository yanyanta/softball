{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Graphics.UI.SDL as SDL
import Data.List as L (foldr, null, filter, sortBy, groupBy)
import Data.Word
import Data.Map as M
import Data.Maybe
import Control.Lens
import Data.Array.IArray as A
import System.Random

-- | Screen informations
cellSide :: Int
cellSide = 36
xCellNum :: Int
xCellNum = 6
yCellNum :: Int
yCellNum = 13
interval :: Int
interval = 15
player1Left :: Int
player1Left = scrPad
player2Left :: Int
player2Left = scrPad + cellSide*xCellNum + interval
scrPad :: Int
scrPad = 5
scrWidth :: Int
scrWidth = 2*(cellSide*xCellNum)+interval+2*scrPad
scrHeight :: Int
scrHeight = cellSide*yCellNum+2*scrPad
emptyOutline :: Outline
emptyOutline = array (0, xCellNum) [(x, 0)| x <- [0 .. xCellNum-1]]

-- | Data declaration part
-- | The 'State' data structure contains game state for eventLoop.
data State = State {
  _player1 :: Player,
  _player2 :: Player,
  _frame :: Frame,
  _screen :: SDL.Surface,
  _images :: Images}
             
data Player = Player {
  _playerID :: PID,
  _ctrlBalls :: Ctrl,
  _heapBalls :: Heap,
  _waitingBalls :: [Ctrl],
  _outline :: Outline,
  _flag :: StateFlag,
  _frameSpan :: Frame}

data PID = PID1 | PID2

data Ctrl = Ctrl {_balls::Balls, _angle::Angle}
               deriving Show

data Ball = Ball {_coor :: Coor, _kind  :: Kind}
      deriving (Show,Eq)

data Images = Images {red  :: SDL.Surface,
                      blue :: SDL.Surface,
                      green:: SDL.Surface,
                      yellow :: SDL.Surface,
                      purple :: SDL.Surface}

data Kind = Red | Blue | Green | Yellow | Purple
          deriving (Show,Eq, Enum, Bounded, Ord)
 
data ArrowKey = LeftKey | DownKey | RightKey | UpKey
              deriving (Eq, Enum, Bounded)

data Angle = Downward | Leftward | Upward | Rightward
           deriving (Show, Eq, Enum, Bounded)

data StateFlag = Control | Clear | Descent | End
               deriving (Show,Eq)

-- | Type declarations
type Balls = [Ball]
type Heap = M.Map Coor Kind
type Outline = Array Int Int
type Coor = (Int, Int)
type Time = Word32
type Frame = Word

-- | Class declaration part
class Obj a where
  pos :: a -> Coor

class Drawable a where
  draw :: State -> PID -> a -> IO ()

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
$(makeLenses ''Player)
$(makeLenses ''Ball)
$(makeLenses ''Ctrl)
x = _1
y = _2

-- | Instance declaration
instance Obj Ball where
  pos = view coor

instance Drawable Ctrl where
  draw state pid ctrl = mapM_ (draw state pid) (view balls ctrl)

instance Drawable Ball where
  draw state pid ball = SDL.blitSurface 
                    (kindToImage (view images state) (view kind ball))
                    (Just $ SDL.Rect 0 0 cellSide cellSide)
                    (view screen state)
                    (Just $ SDL.Rect ((view (coor.x) ball)*cellSide+left)
                     (scrHeight-(((view (coor.y) ball)+1)*cellSide+scrPad))
                     cellSide cellSide) >> return ()
    where
      left = case pid of
        PID1 -> player1Left
        PID2 -> player2Left

instance Drawable Heap where
  draw state pid heap = mapM_ (\(c,k) -> draw state pid (Ball {_coor=c,_kind = k}))
                        (M.toList heap)
  
instance Movable Ball where
    move ball delta ol =
      let newCoor = view coor ball |+| delta in
      if movableCoor newCoor ol
      then Just (set coor newCoor ball) 
      else Nothing

instance Movable Ctrl where
  move ctrl delta ol = do
    movedBalls <- allJust $ Prelude.map (\ball-> move ball delta ol)
                  (view balls ctrl)
    return (set balls movedBalls ctrl)

instance Circular Angle

-- | Function declarations
kindToImage :: Images -> Kind ->SDL.Surface
kindToImage imgs knd = case knd of
  Red    -> red imgs
  Blue   -> blue imgs
  Green  -> green imgs
  Yellow -> yellow imgs
  Purple -> purple imgs

(|+|) :: Coor -> Coor -> Coor
(x1,y1) |+| (x2,y2) = (x1+x2, y1+y2)

inField :: Coor -> Bool
inField c
  | 0 <= view x c && view x c < xCellNum && 0 <= view y c = True
  | otherwise = False

movableCoor :: Coor -> Outline -> Bool
movableCoor c ol
  | inField c && view y c >= ol A.! view x c = True
  | otherwise = False
  
rotate :: [Coor] -> Ctrl -> Outline -> Ctrl
rotate dels ctrl ol =
  let 
    [ker, ano] = view balls ctrl
    ang = view angle ctrl
    nextAno = over coor (|+| (dels !! fromEnum ang)) ano
    nextCtrl = Ctrl {_balls = [ker, nextAno], _angle = csucc ang}
  in
   if movableCoor (view coor nextAno) ol
   then nextCtrl
   else rotate dels nextCtrl ol

rotateR :: Ctrl -> Outline -> Ctrl
rotateR = rotate [(-1, 1), (1, 1), (1, -1), (-1, -1)]  
rotateL :: Ctrl -> Outline -> Ctrl
rotateL = rotate [(1, 1), (-1, 1), (-1, -1), (1, -1)]
                
ctrlGen :: [Ctrl] -> (Ctrl, [Ctrl])
ctrlGen lst =
  case splitAt 1 lst of
    ([newCtrl],rest) -> (newCtrl, rest)
    _ -> error "ERROR at ctrlGen"

getHeapFirst :: Heap -> Maybe (Coor, Kind)
getHeapFirst heap
  | M.null heap = Nothing
  | otherwise   = Just $ M.elemAt 0 heap

insertBallToHeap :: Ball -> Heap -> Heap
insertBallToHeap ball = M.insert (view coor ball) (view kind ball)

-- insertBallsToHeap :: Balls -> Heap -> Heap
-- insertBallsToHeap bs heap =
--   Prelude.foldr (\ball -> insertBallToHeap ball) heap bs

allJust :: [Maybe a] -> Maybe [a]
allJust lst =
  if all isJust lst then Just $ Prelude.map fromJust lst else Nothing

gameinit :: IO State
gameinit = do
       SDL.init [SDL.InitEverything]
       scr <- SDL.setVideoMode scrWidth scrHeight 32 []
       SDL.setCaption "Soft Ball" "Soft Ball"
       gen <- getStdGen 
       let waitings = initCtrl . Prelude.map toEnum $
                      randomRs (0, fromEnum (maxBound :: Kind)) gen
           (initialCtrl, initialWaitingBalls) = ctrlGen waitings
       redBallImage <- SDL.loadBMP "./red.bmp"
       blueBallImage <- SDL.loadBMP "./blue.bmp"
       greenBallImage <- SDL.loadBMP "./green.bmp"
       yellowBallImage <- SDL.loadBMP "./yellow.bmp"
       purpleBallImage <- SDL.loadBMP "./purple.bmp"
       return State {_player1 = Player {_playerID = PID1,
                                        _ctrlBalls = initialCtrl,
                                        _heapBalls = M.empty,
                                        _waitingBalls = initialWaitingBalls,
                                        _outline = emptyOutline,
                                        _flag = Control,
                                        _frameSpan = 1},
                     _player2 = Player {_playerID = PID2,
                                        _ctrlBalls = initialCtrl,
                                        _heapBalls = M.empty,
                                        _waitingBalls = initialWaitingBalls,
                                        _outline = emptyOutline,
                                        _flag = Control,
                                        _frameSpan = 1},
                     _frame = 0,
                     _screen = scr,
                     _images = Images {red = redBallImage,
                                       blue = blueBallImage,
                                       green = greenBallImage,
                                       yellow = yellowBallImage,
                                       purple = purpleBallImage
                                      }
                    }
       
gameend :: IO ()
gameend = SDL.quit >> print "Over"

wait :: IO ()
wait = do
  start <- SDL.getTicks
  SDL.delay 17
      
initCtrl :: [Kind] -> [Ctrl]
initCtrl (l:r:t) =
  Ctrl { _balls = [Ball {_coor = (3, 12), _kind = l},
                       Ball {_coor = (3, 13), _kind = r}],
         _angle = Upward
       } : initCtrl t

initCtrl _ = error "INIT_CTRLBALL"

descent :: Player -> Maybe Player
descent player = do
  movedCtrl <- move (view ctrlBalls player) (0,-1) (view outline player)
  return $ set ctrlBalls movedCtrl player

inc :: Array Int Int -> Int -> Array Int Int
inc ary x = ary//[(x, ary A.! x + 1)]

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
createOutline = M.foldrWithKey countUp emptyOutline
  where
    countUp :: Coor -> Kind -> Outline -> Outline
    countUp c _ o = inc o (view x c)

descentTillLand :: Outline -> Ball -> (Ball, Outline)
descentTillLand ol ball =
  let newCoor = (fst (pos ball),ol A.! fst (pos ball)) in
  (set coor newCoor ball, inc ol (view x newCoor))

freeFall :: Player -> Player
freeFall player =
  player { _ctrlBalls = newCtrl, _heapBalls = newHeap,
           _waitingBalls = newWaiting, _outline = newOutline}
  where
    (newCtrl, newWaiting) = ctrlGen $ view waitingBalls player
    (newHeap, newOutline) =
      L.foldr (\b (h, o) -> let (nb, no) = descentTillLand o b
                          in (insertBallToHeap nb h, no))
      (view heapBalls player, view outline player)
      (sortBy (\b1 b2 -> view (coor.y) b2 `compare` view (coor.y) b1)
       (view balls (view ctrlBalls player)))

normalize :: Heap -> Heap
normalize heap =
  M.fromList $ 
  concatMap (\col -> zipWith (\n c -> let newC = (view x c, n)
                                      in (newC, heap M.! c)) -- kokojanai
                     [0, 1 .. (length col -1)] col)
  (toXcolList heap)
  
getHeap :: Coor -> Heap -> Maybe Kind
getHeap = M.lookup

nextToCoor ::  Heap -> Coor -> Kind -> [Coor]
nextToCoor heap cor knd =
  [nextCoor|d <- [(0,-1),(-1,0),(0,1),(1,0)],
   let
     nextCoor = d |+| cor
     nextKind = getHeap nextCoor heap,
   isJust nextKind,
   knd ==  fromJust nextKind]

delCoor :: Coor -> Heap -> Heap
delCoor cor heap = M.delete cor heap

delCoors :: [Coor] -> Heap -> Heap
delCoors blls heap = L.foldr delCoor heap blls

disapCoors :: Heap -> Coor -> Kind -> [Coor]
disapCoors heap start knd =
  let nexts = nextToCoor heap start knd in
  start : concatMap (\c -> disapCoors (delCoors nexts heap) c knd) nexts

disapCoorss :: Heap -> [[Coor]]
disapCoorss heap =
  case getHeapFirst heap of
    Nothing     -> []
    Just (c, k) ->
      hConnection : disapCoorss (delCoors hConnection heap)
      where
        hConnection = disapCoors (delCoor c heap) c k

clear :: Player -> Maybe Player
clear player =
  if L.null coorsTodisppear then Nothing else Just newPlayer
  where
    connectedList = disapCoorss (view heapBalls player)
    coorsTodisppear = concat . L.filter ((4<=) . length) $ connectedList
    prepare = delCoors coorsTodisppear (view heapBalls player)
    normalizedHeap = normalize $ prepare 
    newPlayer = player {_heapBalls = normalizedHeap,
                      _outline = createOutline normalizedHeap
                     }

defaultUpdate :: State -> State
defaultUpdate state =
  over player2 newFlag . over player1 newFlag . set frame newFrame $ state
  where
    newFrame = view frame state + 1
    newFlag player = case view flag player of
      Control
        | newFrame `mod` 10 == 0 -> set flag Descent player
        | otherwise -> player
      _ -> player

overp :: Player -> Bool
overp player = 13< view outline player A.! 3

drawState :: State -> IO ()
drawState state = do
  black <- SDL.mapRGB (SDL.surfaceGetPixelFormat (view screen state)) 0 0 0
  SDL.fillRect (view screen state) (Just $ SDL.Rect 0 0 scrWidth scrHeight) black
  draw state PID1 (view (player1.ctrlBalls) state)
  draw state PID1 (view (player1.heapBalls) state)
  draw state PID2 (view (player2.ctrlBalls) state)
  draw state PID2 (view (player2.heapBalls) state)
  SDL.flip (view screen state)
 
main :: IO ()
main = gameinit >>= mainLoop

mainLoop :: State -> IO ()
mainLoop state =
  if (view (player1.flag) state == End || view (player2.flag) state == End)
  then gameend
  else
    checkEvent (over player2 playerLoop (over player1 playerLoop state))
    >>= drawWaitUpdateLoop 
  where
    drawWaitUpdateLoop state =
       wait >> drawState state >> mainLoop (defaultUpdate state)
  
playerLoop :: Player -> Player  
playerLoop player =
  if view frameSpan player /= 0 
  then over frameSpan (subtract 1) player
  else
    case view flag player of
      Descent -> case descent player of
        Just descented -> setFrameSpan 40 . set flag Control $ descented
        Nothing    -> freeFall . setFrameSpan 10 . set flag  Clear $ player 
      Clear -> case clear player of
        Just clearedPlayer -> setFrameSpan 40 clearedPlayer
        Nothing
          | overp player -> set flag End player
          | otherwise -> set flag Control player
      _ -> player
  where
    setFrameSpan fspan player =
      set frameSpan fspan player

pidToPlayerLens pid = case pid of
  PID1 -> player1
  PID2 -> player2

checkEvent :: State -> IO State
checkEvent state = do
  event <- SDL.pollEvent
  return $
    case event of
      KeyUp (Keysym key _ _) ->
        helper PID2 key . helper PID1 key $ state
      _ -> state
  where
    helper pid key state =
      let player = pidToPlayerLens pid in
        case view (player.flag) state of
          Clear -> state
          _ ->
            case key of
              SDLK_ESCAPE -> set (player.flag) End state
              SDLK_UP -> set ((player.ctrlBalls))
                         (rotateR (view (player.ctrlBalls) state)
                          (view (player.outline) state)) state
              SDLK_LEFT -> set (player.ctrlBalls)
                           (moveInField (view (player.ctrlBalls) state) (-1, 0)
                            (view (player.outline) state)) state
              SDLK_RIGHT -> set (player.ctrlBalls)
                             (moveInField (view (player.ctrlBalls) state) (1, 0)
                             (view (player.outline) state)) state
              SDLK_DOWN -> over player freeFall $ set (player.flag) Clear state
              _ -> state
