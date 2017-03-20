{-

For your final Elm project, I'm going to ask you to build a video game
in elm that's a little more sophisticated than Spacewar - a
classic-style scrolling shooter game

My personal nostalgic example of this genre is Raptor: Call of the
Shadows.  You can see it here:

https://www.youtube.com/watch?v=ZUfeVcIg3Mg

NOTE: Please make sure it is clear how to play your game.  In the
comments is fine, but I should have to go code spelunking to figure
out what fires bullets.

Here are the requirements:

1.  Graphics are not important, except insofar as they communicate
gameplay elements.  If you want this game to be about heroic green
triangles and their attempt to defend their homeland from the nefarious
red,yellow,and blue triangles I'm fine with that.

But if I can't tell what's a power-up, or all the weapons seem the
same because I can't see how they act differently - that's bad.

Note that if you want to have graphics, I reccommend you host the
graphics on a website you own so that your code runs in the online elm
"try" mode (I think that should work anyways).  In the past, I've had
problems installing students' games. In a pinch though, I'm happy to
demo it on your computer if that's what you need.

2. [33 points] Your game should have the distinctive elements of the
scrolling shooter genre:

   a.  A background that automatically slowly scrolls (could just be
       stars or clouds or something) 

   b.  A flying ship (that's you) that shoots

   c.  Tons of enemies that fly around (usually in prearranged patterns,
       coming and going at specific times)

   d.  You can blow up the enemies, your enemies can blow you up.
       Crashing into enemies blows them up and at least damages you. 

3. [33 points] At least 4 qualitatively different weapons.  These
should not just be different color weapons that do different amounts
of damage.  At the very least, they should fire in highly different
patterns, spawn additional sub-bullets etc.  At best, they should act
differently (area of effect, damage over time, homing, etc.).  The
weapon effects should be clear visibly (obvious for homing, but damage
over time might need a halo effect or something).

Make a cheat or something so I can try all the weapons without getting
tons of powerups.

4. [33 points] At least 4 qualitatively different enemies.  Again,
more different than just different amounts of life.  At the very
least, different patterns of movement and different numbers of bullet
sources/times for shooting.  At best, some simplistic AI or different
attack effects.

The enemies should be visibly distinct.

-}

{-
    to move use arrow keys
    to shoot use space bar
    to automatically switch weapons without getting upgrades press 1-4
    to become invincible press i
-}

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second, minute)
import Keyboard exposing(..)
import AnimationFrame exposing (..)
import Debug exposing (..)
import Random exposing (..)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL
type Monsters = Monsters (Float, Float, Int, Int, Float) ((Float, Int) -> Monsters)
type Updater = Updater (Float, Float, Int) ((Float, Int) -> Updater) 
type alias Model = {x : Float, y : Float, bullets : List Updater, 
              monsters : List Monsters, lastTime : Maybe Time, time : Float
              , stars : List (Int, Float)
              , interval : {m1 : Float, m2 : Float, m3 : Float, m4 : Float}
              , health : Int, weapons : List (Float, Float, Int), curWeapon : Int
              , invincible : Bool, level : Float, pressed : List (String)}


init : (Model, Cmd Msg)
init =
  ({ x = 250
    , y = 450
    , bullets =[]
    , monsters = [monster1 250 0 1 4 0 (0,0)]
    , lastTime=Nothing
    , time = 0
    , stars = []
    , interval = {m1 = 4, m2 = 7, m3 = 10, m4 = 15}
    , health = 5
    , weapons = []
    , curWeapon = 1
    , invincible = False
    , level = 1
    , pressed = []
    }, Cmd.none)

--monsters

monster1 : Float -> Float -> Int -> Int -> Float -> (Float, Int) -> Monsters
monster1 x y c health lifeSpan (delta, dh) =
  let newY = y + delta/10
    in Monsters (x, newY, c, (health - dh), (lifeSpan + delta)) (monster1 x newY c (health - dh) (lifeSpan + delta))

monster2 x y c initialX health lifeSpan (delta, dh) =
  if x > initialX + 50 then
    Monsters (x, y, c, (health - dh), (lifeSpan + delta)) (monster2b x y c x (health - dh) (lifeSpan + delta) )
  else
    let newX = x + delta/20
        newY = y + delta/10
      in Monsters (newX, newY, c, (health - dh), (lifeSpan + delta) ) (monster2  newX newY c initialX (health - dh) (lifeSpan + delta))

monster2b x y c initialX health lifeSpan (delta, dh) =
  if x < initialX - 50 then
    Monsters (x, y, c, (health - dh), (lifeSpan + delta)) (monster2 x y c x (health - dh) (lifeSpan + delta))
  else
    let newX = x - delta/20   
        newY = y + delta/10
      in Monsters (newX, newY, c, (health - dh), (lifeSpan + delta)) (monster2b  newX newY c initialX (health - dh) (lifeSpan + delta))

monster3 x y xp yp c health lifeSpan (delta, dh) =
  let newX = (if x == xp then
              x
            else if x < xp then
              x + delta/7
            else
              x - delta/7)
      newY = y + delta/10
  in Monsters (newX, newY, c, (health - dh), (lifeSpan + delta)) 
              (monster3 newX newY xp yp c (health-dh) (lifeSpan + delta))

monster4 x y c health lifeSpan (delta, dh) =
  let newY = y + delta/10
  in
  if (y > 300) then
   Monsters (x, newY, c, (health-dh), (lifeSpan+delta)) (monster4b x newY c (health-dh) (lifeSpan+delta))
  else
    Monsters (x, newY, c, (health-dh), (lifeSpan+delta)) (monster4 x newY c (health-dh) (lifeSpan+delta))

monster4b x y c health lifeSpan (delta, dh) =
  if lifeSpan < 15000 then
    Monsters (x, y, c,(health-dh), (lifeSpan+delta)) (monster4b x y c (health-dh) (lifeSpan+delta))
  else
    Monsters (x, y, c,(health-dh), (lifeSpan+delta)) (monster1 x y c (health-dh) (lifeSpan+delta))


--bullets
bullet1 : Float -> Float -> Int -> (Float, Int) -> Updater
bullet1 x y health (delta, dh) =
  let newX = x 
      newY = y - delta/7
    in Updater (newX, newY, (health - dh)) (bullet1 newX newY (health - dh)) 

mbullet1 : Float -> Float -> Int -> (Float, Int) -> Updater
mbullet1 x y health (delta, dh) =
  let newX = x 
      newY = y + delta/7
    in Updater (newX, newY, (health - dh)) (mbullet1 newX newY (health - dh)) 

bullet2 : Float -> Float -> Float -> Float -> Int -> Float -> (Float, Int) -> Updater
bullet2 x y xc yc health lifeSpan (delta, dh) =
  let newX = x - delta/7
      newY = yc - (sqrt (abs (50^2 - (newX - xc)^2)))
      newHealth = if lifeSpan > 5000 then
                      0
                  else
                    health - dh
  in
  if newX <= xc - 50 then
     Updater (newX, newY, newHealth) (bullet2b newX newY xc yc newHealth (lifeSpan + delta))
    else
   Updater (newX, newY, newHealth) (bullet2 newX newY xc yc newHealth (lifeSpan + delta))

bullet2b : Float -> Float -> Float -> Float -> Int -> Float -> (Float, Int) -> Updater
bullet2b x y xc yc health lifeSpan (delta, dh) =
  let newX = x + delta/7
      newY = yc + (sqrt (abs (50^2 - (newX - xc)^2)))
      newHealth = if lifeSpan > 5000 then
                      0
                  else
                    health - dh
  in
  if newX >= xc + 50 then
     Updater (newX, newY, newHealth) (bullet2 newX newY xc yc newHealth (lifeSpan + delta))
    else
   Updater (newX, newY, newHealth) (bullet2b newX newY xc yc newHealth (lifeSpan + delta))

bullet3 x y xp yp xm ym health lifeSpan (delta, dh) =
  let 
    newX = (if x == xm then
              x
            else if x < xm then
              x + delta/7
            else
              x - delta/7)
    newY = y - delta/7
  in
    Updater (newX, newY, health - dh) (bullet3 newX newY xp yp xm ym (health - dh) lifeSpan)

bullet4create x y =
    bullet4 (x+10) y (x+10) y 3 (0,0)

--bullet4 : Float -> Float -> Float -> Float
bullet4 x y initialX initialY health (delta, dh) =
    let newY = y - delta/25
        deltaY = newY - initialY
        newX = initialX + 30*sin (deltaY/7)
    in Updater (newX, newY, (health - dh)) (bullet4 newX newY initialX initialY (health - dh))
 


-- UPDATE

type Msg
  = Key Int | Tick Time | BackgroundUpdate Time | NewStar Int
    | NewMonster Int Float  | RandMonster Int Time | Bullet (List (Float, Float)) Time
    | NewWeapon Time | DropNewWeapon (Float, Float) Int | NewWep (Float, Float)
    | Downgrade Time | Levelup Time | Up Int

updatePos : Float -> Updater -> Updater
updatePos deltaT updater = 
 case updater of
   Updater _ func -> func (deltaT,0)

monsterUpdate : Float -> Monsters -> Monsters
monsterUpdate deltaT updater =
  case updater of
    Monsters _ func -> func (deltaT,0) 

updateStar deltaT star =
  case star of
    (x,y) -> (x, (y + 0.5))

onScreen star =
  case star of
    (x,y) -> y < 600 && y > 0

getShots monster =
  case monster of
    Monsters (x,y,1, _, lifeSpan) _ -> 
      if ((Basics.floor lifeSpan) % 3000) < 100 then
        (x, y + 30)
      else 
        (0,0)
    Monsters (x,y,2, _, lifeSpan) _ -> 
      if ((Basics.floor lifeSpan) % 3000) < 300 then
        (x, y + 30)
      else 
        (0,0)
    Monsters (x,y,3, _, lifeSpan) _ -> 
      if ((Basics.floor lifeSpan) % 1000) < 100 || ((Basics.floor lifeSpan) % 5500) < 100 || ((Basics.floor lifeSpan) % 6000) < 100 then
        (x, y + 30)
      else 
        (0,0)
    Monsters (x,y,4, _, lifeSpan) _ -> 
      if ((Basics.floor lifeSpan) % 1000) < 100 || ((Basics.floor lifeSpan) % 1500) < 100 then
        (x, y + 30)
      else 
        (0,0)
    Monsters (x,y,t, _, lifeSpan) _ -> 
      if ((Basics.floor lifeSpan) % 500) < 7 then
        (x, y + 30)
      else 
        (0,0)

getValues monst =
  case monst of
    Monsters (x,y,_,_,_) _ -> (x,y)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
   case model.lastTime of
        Just oldTime -> (let 
                             bull =
                            model.bullets
                                  |> List.map (updatePos (newTime - oldTime))
                                  |> updateBul model.monsters
                                  |> List.filter rmvBullets
                             monst =
                                  model.monsters
                                  |> List.map (monsterUpdate (newTime- oldTime))
                                  |> upd model.bullets
                                  |> List.filter removeDead 
          in {model | lastTime= Just newTime
                                , x = if List.member "left" model.pressed then
                                    model.x - 3
                                    else if List.member "right" model.pressed then
                                        model.x + 3
                                    else
                                        model.x
                                ,  y = if List.member "up" model.pressed then
                                    model.y - 3
                                    else if List.member "down" model.pressed then
                                        model.y + 3
                                    else
                                        model.y
                                , stars = List.filter onScreen (List.map (updateStar (newTime - oldTime)) model.stars)
                                , bullets = List.filter  (hit1 model.x model.y) bull
                                , monsters = List.filter (hitPlayer model.x model.y) monst
                                , health = (if not(List.isEmpty (List.filter (hitPlayer1 model.x model.y) monst))  then
                                      model.health - 2
                                   else if not(List.isEmpty (List.filter (hit model.x model.y) bull)) then
                                     model.health - 1
                                    else
                                      model.health)
                                , curWeapon = 
                                           case List.head (List.map (getWeapon model.x model.y model.curWeapon) model.weapons) of
                                            Just val -> val
                                            Nothing -> model.curWeapon
                                , weapons = if List.isEmpty model.weapons then 
                                          model.weapons
                                          else
                                            List.filter (test model.x model.y) model.weapons
                                }, Cmd.none)
        Nothing -> ({model | lastTime=Just newTime},  Cmd.none)
    Key 37 -> 
            ({model | pressed = "left" :: model.pressed}, Cmd.none)
    Key 39 -> 
            ({model | pressed = "right" :: model.pressed}, Cmd.none)
    Key 40 -> 
            ({model | pressed = "down" :: model.pressed}, Cmd.none)
    Key 38 -> 
            ({model | pressed = "up" :: model.pressed}, Cmd.none)
    Up 37 -> 
            ({model | pressed = List.filter (rem "left") model.pressed}, Cmd.none)
    Up 39 -> 
            ({model | pressed = List.filter (rem "right") model.pressed}, Cmd.none)
    Up 40 -> 
            ({model | pressed = List.filter (rem "down") model.pressed}, Cmd.none)
    Up 38 -> 
            ({model | pressed = List.filter (rem "up") model.pressed}, Cmd.none)
    Up x -> (model, Cmd.none)
    Key 32-> 
            case model.curWeapon of 
              1 -> ({model | bullets = (bullet1 (model.x + 10) model.y 1 (0,0)) :: model.bullets}, Cmd.none)
              2 -> ({model | bullets = (bullet2 (model.x + 40) (model.y - 5) (model.x + 10) (model.y + 5) 1 0 (0,0)) :: model.bullets}, Cmd.none)
              3 -> ({model | bullets = (case List.head model.monsters of
                                          Just monst -> let (x,y) = getValues monst
                                                        in (bullet3 model.x (model.y - 10) model.x (model.y - 10) x y 4 0 (0,0) )
                                          Nothing -> (bullet1 (model.x + 10) model.y 1 (0,0)))

                :: model.bullets}, Cmd.none)
              4 -> ({model | bullets = (bullet4create model.x (model.y - 10)) :: model.bullets}, Cmd.none)
              _ -> ({model | bullets = (bullet2 (model.x + 40) (model.y - 5) (model.x + 10) (model.y + 5) 1 0 (0,0)) :: model.bullets}, Cmd.none)
    Key 73 -> 
            ({model | invincible = not(model.invincible)}, Cmd.none)
    Key 49 ->
            ({model | curWeapon = 1}, Cmd.none)
    Key 50 ->
            ({model | curWeapon = 2}, Cmd.none)
    Key 51 ->
            ({model | curWeapon = 3}, Cmd.none)
    Key 52 ->
            ({model | curWeapon = 4}, Cmd.none)
    Key y -> Debug.log (toString y)
            (model, Cmd.none)
    BackgroundUpdate x->
            (model, (Random.generate NewStar (Random.int 1 600)))
    NewStar x ->
            ({model | stars = (x,0) :: model.stars}, Cmd.none)
    RandMonster y x ->
            (model, (Random.generate (NewMonster y) (Random.float 1 600)))
    NewMonster x y->  --x y xp yp c health lifeSpan (delta, dh)
            case x of
              1 -> ({model | monsters = (monster1 y 0 1 4 0 (0,0)) :: model.monsters}, Cmd.none)
              2 -> ({model | monsters = (monster2 y 0 2 y  4 0 (0,0)) :: model.monsters}, Cmd.none)
              3 -> ({model | monsters = (monster3 y 0 model.x model.y 3 4 0 (0,0)) :: model.monsters}, Cmd.none)
              4 -> ({model | monsters = (monster4 y 0 1 4 0 (0,0)) :: model.monsters}, Cmd.none)
              _ -> (model, Cmd.none)
    Bullet list x ->
           ({model | bullets = List.append model.bullets (List.map toBullets (List.filter notTime list))} , Cmd.none)
    NewWeapon x ->
            (model, Random.generate NewWep (Random.pair (Random.float 100 500) (Random.float 300 500)))
    NewWep (a,b) ->
            (model, Random.generate (DropNewWeapon (a,b)) (Random.int 2 4))
    DropNewWeapon (a,b) x ->
            ({model | weapons = [(a,b,x)]}, Cmd.none)
    Downgrade x ->
            ({model | curWeapon = 1}, Cmd.none)
    Levelup x ->
            ({model | level = model.level + 0.3}, Cmd.none)
rem str val =
    not(str == val)
--handel weapon changes
getWeapon xp yp curWep weapon = 
  case weapon of
    (x,y,w) -> if not (collision xp yp 20 10 x y 10 10) then
                  curWep
               else
                 w

test xp yp weapon =
  case weapon of
    (x,y,_) -> not (collision xp yp 20 10 x y 10 10)


--handel player/bullet collision
hit xp yp bul =
  case bul of
    Updater (x,y,h) _ -> (collision xp yp 20 10 x y 2 10)
hit1 xp yp bul =
  case bul of
    Updater (x,y,h) _ -> not(collision xp yp 20 10 x y 2 10)
notTime pos =
  case pos of
    (0,0) -> False
    (x,y) -> True
   -- x -> False

toBullets positions = 
  case positions of
    (x,y) -> mbullet1 x y 1 (0,0)

--handel player/monster collision
hitPlayer px py monster =
  case monster of
    Monsters (x,y,c,h, _) _ -> not (collision (x-10) (y-10) 20 20 px py 20 10)
hitPlayer1 px py monster =
  case monster of
    Monsters (x,y,c,h,_) _ -> (collision (x-10) (y-10) 20 20 px py 20 10)

--handel removal of dead bullets
rmvBullets bullet =
  case bullet of
    Updater (x,y,h) _ -> (h > 0) && y > -10 && y < 610


updateBul monsters bullets =
  bullets
  |> List.map (func1 monsters)

func1 monsters bullet =
  if bulletHit monsters bullet then
    updateBulHealth bullet
  else
    bullet

bulletHit monster bullet =
  case bullet of 
    Updater (x,y,h) _ -> not (List.isEmpty (List.filter (collidem x y) monster))

updateBulHealth bullet =
  case bullet of
    Updater _ func -> func (0,1)

collidem bx by monster =
  case monster of
     Monsters (x,y,s,h,_) _ -> (collision (x - 10) (y - 10) 20 20 bx by 2 10)

--handel monsters being hit
removeDead monster =
  case monster of
    Monsters (x,y,c,h,_) _ -> (h > 0) && (y < 600)

updateHealth monster =
  case monster of
    Monsters _ func-> func (0,1)

upd bullets monsters = monsters
  |> List.map (func bullets)

func bullets monster =
  if monsterHit bullets monster then
    updateHealth monster
  else
    monster

monsterHit bullet monster =
  case monster of
    Monsters (x,y,c,h,_) _ -> not (List.isEmpty (List.filter (collide x y) bullet))

collide mx my bullet =
  case bullet of 
    Updater (x,y,h) _-> (collision (mx - 10) (my - 10) 20 20 x y 2 10)


--tests if two items have hit
--bullet is second item
collision x1 y1 w1 h1 x2 y2 w2 h2 =
  (((x1 < x2) && ((x1 + w1) > x2)) && ((y1 < y2) && ((y1 + h1) > y2))) ||
    (((x1 < (x2 + w1)) && ((x1 + w1) > (x2 + w1))) && ((y1 < y2) && ((y1 + h1) > y2)))


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.batch
  [
    AnimationFrame.times Tick
  , Keyboard.downs (\k -> Key k)
  , Keyboard.ups (\k -> Up k)
 -- , Keyboard.keysDown Press 
  --creates the stars
  , Time.every (0.1 * second) BackgroundUpdate 
  , Time.every ((model.interval.m1 / model.level)* second) (RandMonster 1)
  , Time.every ((model.interval.m2 / model.level)* second) (RandMonster 2)
  , Time.every ((model.interval.m3/ model.level) * second) (RandMonster 3)
  , Time.every ((model.interval.m4/ model.level) * second) (RandMonster 4)
  , Time.every (0.1 * second) (Bullet (List.map getShots model.monsters))
  , Time.every (20 * second) NewWeapon
  , Time.every (10 * second) Downgrade
  , Time.every (30 * second) Levelup
  ]

getColor health =
  case health of
    1 -> "#381579"
    2 -> "#521579"
    3 -> "#A9156A"
    4 -> "#FF1C9E"
    y -> "#333333"


-- VIEW  

posToBullet: Updater -> Svg Msg
posToBullet updater =
 case updater of
   Updater (x1,y1,h) _ -> rect [x (toString x1), y (toString (y1 - 10)), width "2", height "5", fill "#2EE11E"] []

draw : Monsters -> Svg Msg
draw monst = 
  case monst of
   Monsters (x,y,c,h,_)  _ -> circle [ cx (toString x), cy (toString y), r "10", fill (getColor h) ] []

background star =
  case star of
    (x,y) -> circle [ cx (toString x), cy (toString y), r "1", fill "#FFFFFF"] []

drawWeapon weap =
  case weap of
    (x1,y1,4) -> rect [x (toString x1), y (toString y1), width "10", height "10", fill "#F3296C"] []
    (x1,y1,3) -> rect [x (toString x1), y (toString y1), width "10", height "10", fill "#00ECFB"] []
    (x1,y1,2) -> rect [x (toString x1), y (toString y1), width "10", height "10", fill "#C6F329"] []
    (x1,y1,_) -> rect [x (toString x1), y (toString y1), width "10", height "10", fill "#5729F3"] []

view : Model -> Html Msg
view model =
    svg [viewBox "0 0 600 600", width "600px"]
      (if model.health <= 0 && not model.invincible then
        [rect [x "0", y "0", width "700", height "700", fill "#000000"] []]
      else 
          ([rect [x "0", y "0", width "700", height "700", fill "#000000"] []] ++
        ((List.map background model.stars) ++ 
        ((List.map drawWeapon model.weapons)  ++
           ([rect [x (toString model.x), y (toString model.y), width "20", height "10", fill "#0B79CE"] [] ] ++
            ((List.map posToBullet model.bullets) ++  (List.map draw model.monsters))
        )))))