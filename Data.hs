module Data where

import BasePrelude
import Base
import Data.Vector.Generic as V

--  Test "nation".
abc :: CountVec
abc = V.fromList [6003,6002,6001]


--  American data.

states50Full :: [String]
''  = ["Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"]

census2010 :: CountVec
census2010 = V.fromList [4802982, 721523, 6412700, 2926229, 37341989, 5044930, 3581628, 900877, 18900773, 9727566, 1366862, 1573499, 12864380, 6501582, 3053787, 2863813, 4350606, 4553962, 1333074, 5789929, 6559644, 9911626, 5314879, 2978240, 6011478, 994416, 1831825, 2709432, 1321445, 8807501, 2067273, 19421055, 9565781, 675905, 11568495, 3764882, 3848606, 12734905, 1055247, 4645975, 819761, 6375431, 25268418, 2770765, 630337, 8037736, 6753369, 1859815, 5698230, 568300]

