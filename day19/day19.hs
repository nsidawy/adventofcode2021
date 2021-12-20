import           Control.Monad
import qualified Data.List       as L
import qualified Data.Map       as M
import qualified Data.Set       as S
import Text.Printf
import Data.List.Split
import Text.Read

type Coord = (Int,Int,Int)
type DistanceMap = M.Map Float (Coord,Coord)
type ScannerDistances = M.Map Int DistanceMap
type Translate = Coord -> Coord

main = do
    scanners <- getInput "input.txt"
    let scannerDistances = M.fromList [(s, getDistanceMap cs) | (s,cs) <- scanners]
    let done = S.fromList [0]
    (scanners, coords) <- solve scannerDistances done []
    print $ length coords
    print $ maximum [getManhattenDistance a b | a <- scanners, b <- scanners]

getManhattenDistance :: Coord -> Coord -> Int
getManhattenDistance a b = dx + dy + dz
    where
        (dx,dy,dz) = getDiffs a b

solve :: ScannerDistances -> S.Set Int -> [Coord] -> IO ([Coord], [Coord])
solve sd d s
    | M.size sd == S.size d = do
        let beacons = L.nub $ concat $ concat [[[c1,c2] | (_,(c1,c2)) <- M.toList dm] | (_, dm) <- M.toList sd]
        return (s, beacons)
    | otherwise = do
    --We assume distances between beacon pairs is unique. If two scanners 
    --have >= 66 (12 choose 2) pairs of beacons with matching distances within
    --their ranges, then we can assume these are matching scanners.
    let (baseScannerId, matchScannerId, matchDistances) = head $ concat [
            [(cur,n,m) | n <- M.keys sd, 
                not $ n `S.member` d,
                let m = getScannerMatches sd cur n,
                length m >= 66]
            | cur <- S.toList d]
    let (scanner, translate) = getScannerAndTranslation matchDistances sd baseScannerId matchScannerId
    --Update the matching scanner Ids coordinates with the translation function
    let m' = M.fromList [(d,(translate c1, translate c2)) | (d,(c1,c2)) <- M.toList $ sd M.! matchScannerId]
    let sd' = M.insert matchScannerId m' sd
    --Add the matched scanner ID to the done set
    let d' = S.insert matchScannerId d
    solve sd' d' (scanner:s)

getScannerAndTranslation :: [Float] -> ScannerDistances -> Int -> Int -> (Coord, Translate)
getScannerAndTranslation (d:ds) sd base other 
    --If multiple rotations match the base coordinates, look at the next pair
    | length matchingRotationI > 1 = getScannerAndTranslation ds sd base other
    | otherwise = ((dx,dy,dz),translate)
    where 
        (a1,b1) = (sd M.! base) M.! d
        (a2,b2) = (sd M.! other) M.! d
        allRotations = zip (getAllRotations a2) (getAllRotations b2) ++ zip (getAllRotations b2) (getAllRotations a2)
        matchingRotationI = getMatchingRotations (a1,b1) allRotations 0
        (rc,_) = allRotations !! head matchingRotationI
        (dx,dy,dz) = getDiffs rc a1
        translate c = 
            let (rx,ry,rz) = (getAllRotations c !! (head matchingRotationI `mod` 24))
            in (rx+dx,ry+dy,rz+dz)

--Find the matching rotations by comparing the diffs of coordinate pairs with the base pair
getMatchingRotations :: (Coord,Coord) -> [(Coord,Coord)] -> Int -> [Int]
getMatchingRotations _ [] _ = []
getMatchingRotations (a1,b1) ((a2,b2):abs) i
    | getDiffs a1 b1 == getDiffs a2 b2 = i : rest
    | otherwise = rest
    where 
        rest = getMatchingRotations (a1,b1) abs (i+1)

getDiffs :: Coord -> Coord -> Coord
getDiffs (x1,y1,z1) (x2,y2,z2) = (x2-x1,y2-y1,z2-z1)

getAllRotations :: Coord -> [Coord]
getAllRotations (x,y,z) = [
        (x,y,z), (x,z,ny), (x,ny,nz), (x,nz,y),
        (nx,y,nz), (nx,nz,ny), (nx,ny,z), (nx,z,y),
        (y,nx,z), (y,z,x), (y,x,nz), (y,nz,nx),
        (ny,nx,nz), (ny,nz,x), (ny,x,z), (ny,z,nx),
        (z,y,nx), (z,nx,ny), (z,ny,x), (z,x,y),
        (nz,y,x), (nz,x,ny), (nz,ny,nx), (nz,nx,y)
    ]
    where
        nx = -1*x
        ny = -1*y
        nz = -1*z
        

getScannerMatches :: ScannerDistances -> Int -> Int -> [Float]
getScannerMatches m a b = M.keys (m M.! a) `L.intersect` M.keys (m M.! b)

getDistanceMap :: [Coord] -> M.Map Float (Coord,Coord)
getDistanceMap cs = M.fromList $ L.nubBy (\a b -> fst a == fst b) [(sqrt . fromIntegral $ (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2, ((x1,y1,z1), (x2,y2,z2))) | 
            (x1,y1,z1) <- cs,
            (x2,y2,z2) <- cs,
            (x1,y1,z1) /= (x2,y2,z2)]

getInput :: String -> IO [(Int, [Coord])]
getInput path = do
    lines <- lines <$> readFile path
    return $ map parseScanner $ splitWhen (== "") lines

parseScanner :: [String] -> (Int, [Coord])
parseScanner lines = (read $ words (head lines) !! 2, map readCoords $ tail lines)

readCoords :: String -> (Int,Int,Int)
readCoords line = (x,y,z)
    where
        [x,y,z] = map read $ splitOn "," line
