
import unittest
import itertools

import ptypes


class RangeTypeTest(unittest.TestCase):
    def setUp(self):
        self.fourTen = ptypes.Range(4, 10)
        self.fourNil = ptypes.Range(4, ptypes.nil)
        self.threeNegOne = ptypes.Range(3, -1)
        self.nilFive = ptypes.Range(ptypes.nil, 5)
        
        self.zero = ptypes.Scalar("0")
        self.five = ptypes.Scalar("5")
        self.hundred = ptypes.Scalar("100")
        self.pi = ptypes.Scalar("3.14")
        self.someList = ptypes.List([self.five, ptypes.Scalar("9")])
        self.somePattern = ptypes.Pattern("6")
        self.someBlock = ptypes.Block([])
    
    @unittest.skip("Constructor test TODO")
    def test_constructor(self):
        pass
    
    def test_copy(self):
        self.assertEqual(self.fourTen, self.fourTen.copy())
        self.assertEqual(self.fourNil, self.fourNil.copy())
        self.assertEqual(self.threeNegOne, self.threeNegOne.copy())
        self.assertEqual(self.nilFive, self.nilFive.copy())
    
    def test_getbounds(self):
        self.assertEqual(self.fourTen.getLower(), 4)
        self.assertEqual(self.fourTen.getUpper(), 10)
        self.assertEqual(self.fourNil.getLower(), 4)
        self.assertIsNone(self.fourNil.getUpper())
        self.assertEqual(self.threeNegOne.getLower(), 3)
        self.assertEqual(self.threeNegOne.getUpper(), -1)
        self.assertIsNone(self.nilFive.getLower())
        self.assertEqual(self.nilFive.getUpper(), 5)
    
    def test_empty(self):
        self.assertFalse(self.fourTen.isEmpty())
        self.assertFalse(self.fourNil.isEmpty())
        self.assertTrue(self.threeNegOne.isEmpty())
        self.assertFalse(self.nilFive.isEmpty())
    
    def test_finite(self):
        self.assertTrue(self.fourTen.isFinite())
        self.assertFalse(self.fourNil.isFinite())
        self.assertTrue(self.threeNegOne.isFinite())
        self.assertTrue(self.nilFive.isFinite())
    
    def test_stringification(self):
        # Using space-separated output format for str
        ptypes.List.outFormat = "s"
        self.assertEqual(str(self.fourTen), "4 5 6 7 8 9")
        self.assertEqual(repr(self.fourTen), "(4,10)")
        self.assertEqual(str(self.fourNil), "(4,())")
        self.assertEqual(repr(self.fourNil), "(4,())")
        self.assertEqual(str(self.threeNegOne), "")
        self.assertEqual(repr(self.threeNegOne), "(3,-1)")
        self.assertEqual(str(self.nilFive), "0 1 2 3 4")
        self.assertEqual(repr(self.nilFive), "((),5)")
    
    @unittest.skip("Bool test TODO")
    def test_bool(self):
        pass
    
    @unittest.skip("Equality test TODO")
    def test_equality(self):
        pass
    
    def test_length(self):
        self.assertEqual(len(self.fourTen), 6)
        # Length of infinite Range raises an error
        with self.assertRaises(ValueError):
            len(self.fourNil)
        self.assertEqual(len(self.threeNegOne), 0)
        self.assertEqual(len(self.nilFive), 5)
    
    def test_tonumber(self):
        self.assertEqual(self.fourTen.toNumber(), [4, 5, 6, 7, 8, 9])
        # Inf range to number should probably be a ValueError, but maybe
        # a generator makes sense; decide on an approach and then uncomment
        #with self.assertRaises(ValueError):
        #    self.fourNil.toNumber()
        self.assertEqual(self.threeNegOne.toNumber(), [])
        self.assertEqual(self.nilFive.toNumber(), [0, 1, 2, 3, 4])
    
    def test_contains(self):
        self.assertNotIn(self.zero, self.fourTen)
        self.assertIn(self.five, self.fourTen)
        self.assertNotIn(self.hundred, self.fourTen)
        self.assertNotIn(self.pi, self.fourTen)
        
        self.assertNotIn(self.zero, self.fourNil)
        self.assertIn(self.five, self.fourNil)
        self.assertIn(self.hundred, self.fourNil)
        self.assertNotIn(self.pi, self.fourNil)
        
        self.assertNotIn(self.zero, self.threeNegOne)
        self.assertNotIn(self.five, self.threeNegOne)
        self.assertNotIn(self.hundred, self.threeNegOne)
        self.assertNotIn(self.pi, self.threeNegOne)
        
        self.assertIn(self.zero, self.nilFive)
        self.assertNotIn(self.five, self.nilFive)
        self.assertNotIn(self.hundred, self.nilFive)
        self.assertIn(self.pi, self.nilFive)
        
        self.assertNotIn(self.fourTen, self.fourNil)
        self.assertNotIn(self.someList, self.fourTen)
        self.assertNotIn(self.somePattern, self.fourTen)
        self.assertNotIn(self.someBlock, self.fourTen)
        self.assertNotIn(ptypes.nil, self.fourNil)
        self.assertNotIn(ptypes.nil, self.nilFive)
    
    def test_topytype(self):
        self.assertEqual(self.fourTen.toSlice(), slice(4, 10))
        self.assertEqual(self.fourTen.toRange(), range(4, 10))
        self.assertEqual(self.fourNil.toSlice(), slice(4, None))
        # Infinite Range to range raises an error
        with self.assertRaises(ValueError):
            self.fourNil.toRange()
        self.assertEqual(self.threeNegOne.toSlice(), slice(3, -1))
        self.assertEqual(self.threeNegOne.toRange(), range(3, -1))
        self.assertEqual(self.nilFive.toSlice(), slice(None, 5))
        self.assertEqual(self.nilFive.toRange(), range(5))
    
    def test_iter(self):
        self.assertEqual(list(self.fourTen),
                         list(map(ptypes.Scalar, [4, 5, 6, 7, 8, 9])))
        self.assertEqual(list(itertools.islice(iter(self.fourNil), 100)),
                         list(map(ptypes.Scalar, range(4, 104))))
        self.assertEqual(list(self.threeNegOne), [])
        self.assertEqual(list(self.nilFive),
                         list(map(ptypes.Scalar, [0, 1, 2, 3, 4])))
    
    @unittest.skip("Getitem test TODO")
    def test_getitem(self):
        pass
    
    def test_count(self):
        self.assertEqual(self.fourTen.count(self.zero), 0)
        self.assertEqual(self.fourTen.count(self.five), 1)
        self.assertEqual(self.fourTen.count(self.hundred), 0)
        self.assertEqual(self.fourTen.count(self.pi), 0)
        
        self.assertEqual(self.fourNil.count(self.zero), 0)
        self.assertEqual(self.fourNil.count(self.five), 1)
        self.assertEqual(self.fourNil.count(self.hundred), 1)
        self.assertEqual(self.fourNil.count(self.pi), 0)
        
        self.assertEqual(self.threeNegOne.count(self.zero), 0)
        self.assertEqual(self.threeNegOne.count(self.five), 0)
        self.assertEqual(self.threeNegOne.count(self.hundred), 0)
        self.assertEqual(self.threeNegOne.count(self.pi), 0)
        
        self.assertEqual(self.nilFive.count(self.zero), 1)
        self.assertEqual(self.nilFive.count(self.five), 0)
        self.assertEqual(self.nilFive.count(self.hundred), 0)
        self.assertEqual(self.nilFive.count(self.pi), 1)
        
        self.assertEqual(self.fourNil.count(self.fourTen), 0)
        self.assertEqual(self.fourTen.count(self.someList), 0)
        self.assertEqual(self.fourTen.count(self.somePattern), 0)
        self.assertEqual(self.fourTen.count(self.someBlock), 0)
        self.assertEqual(self.fourNil.count(ptypes.nil), 0)
        self.assertEqual(self.nilFive.count(ptypes.nil), 0)
    
    @unittest.skip("Index test TODO")
    def test_index(self):
        pass


if __name__ == "__main__":
    unittest.main()
