-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

module TestTruthTable (allTests) where

import Test.HUnit.Base (Test(..), (~=?), (~:), (~?))
import TruthTable

allTests :: Test
allTests = "Test TruthTable"
        ~: TestList
         [ testTruthTable
         , testAssignments
         ]

testTruthTable :: Test
testTruthTable = "Test truthTable"
              ~: TestList
               . map (\(e, n)
                      -> show n
                      ~: e
                     ~=? truthTable n)
                 $ zip expected [1..]
    where expected = [[[True]
                      ,[False]
                      ]
                     ,[[True,  True]
                      ,[True,  False]
                      ,[False, True]
                      ,[False, False]
                      ]
                     ,[[True,  True,  True ]
                      ,[True,  True,  False]
                      ,[True,  False, True ]
                      ,[True,  False, False]
                      ,[False, True,  True ]
                      ,[False, True,  False]
                      ,[False, False, True ]
                      ,[False, False, False]
                      ]
                     ]

testAssignments :: Test
testAssignments =  "Test truthTable"
               ~: TestList
                . map (\(e, vars)
                       -> show vars
                       ~: e
                      ~=? assignments vars
                      )
                $ tests
    where tests = [([[('p', True)]
                    ,[('p', False)]
                    ]
                   ,take 1 vars
                   )
                   ,([[('p', True ), ('q', True )]
                     ,[('p', True ), ('q', False)]
                     ,[('p', False), ('q', True )]
                     ,[('p', False), ('q', False)]
                     ]
                    ,take 2 vars
                    )
                   ,([[('p', True ), ('q', True ), ('r', True )]
                     ,[('p', True ), ('q', True ), ('r', False)]
                     ,[('p', True ), ('q', False), ('r', True )]
                     ,[('p', True ), ('q', False), ('r', False)]
                     ,[('p', False), ('q', True ), ('r', True )]
                     ,[('p', False), ('q', True ), ('r', False)]
                     ,[('p', False), ('q', False), ('r', True )]
                     ,[('p', False), ('q', False), ('r', False)]
                     ]
                    ,take 3 vars
                    )
                   ]
          vars = ['p', 'q', 'r']
