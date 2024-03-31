
import unittest
import itertools

import ptypes


class ListTypeTest(unittest.TestCase):
    def setUp(self):
        self.zero = ptypes.Scalar("0")
        self.emptyList = ptypes.List([])
        self.list123 = ptypes.List([ptypes.Scalar(i) for i in range(1, 4)])
        self.rangeThreeFive = ptypes.Range(3, 5)
        self.rangeFourNil = ptypes.Range(4, ptypes.nil)
        self.somePattern = ptypes.Pattern(".")
        self.emptyBlock = ptypes.Block([])
    
    # TODO: test all the other stuff in the class
    
    def test_append(self):
        testList = ptypes.List([])
        testList.append(self.zero)
        testList.append(self.list123)
        testList.append(self.rangeThreeFive)
        testList.append(self.rangeFourNil)
        testList.append(self.somePattern)
        testList.append(self.emptyBlock)
        testList.append(ptypes.nil)
        self.assertEqual(testList, ptypes.List([self.zero,
                                                self.list123,
                                                self.rangeThreeFive,
                                                self.rangeFourNil,
                                                self.somePattern,
                                                self.emptyBlock,
                                                ptypes.nil]))
        with self.assertRaises(TypeError):
            testList.append(42)
        with self.assertRaises(TypeError):
            testList.append("xyz")
        with self.assertRaises(TypeError):
            testList.append([])
        with self.assertRaises(TypeError):
            testList.append(range(2))
        with self.assertRaises(TypeError):
            testList.append(slice(3, 5))
        with self.assertRaises(TypeError):
            testList.append(None)
    
    def test_extend(self):
        testList = ptypes.List([])
        testList.extend(self.zero)
        testList.extend(self.emptyList)
        testList.extend(self.list123)
        testList.extend(self.rangeThreeFive)
        testList.extend([ptypes.nil])
        testList.extend(map(ptypes.Scalar, "ab"))
        self.assertEqual(testList, ptypes.List([ptypes.Scalar("0"),
                                                ptypes.Scalar("1"),
                                                ptypes.Scalar("2"),
                                                ptypes.Scalar("3"),
                                                ptypes.Scalar("3"),
                                                ptypes.Scalar("4"),
                                                ptypes.nil,
                                                ptypes.Scalar("a"),
                                                ptypes.Scalar("b")]))
        
        with self.assertRaises(ValueError):
            testList.extend(self.rangeFourNil)
        with self.assertRaises(TypeError):
            testList.extend(self.somePattern)
        with self.assertRaises(TypeError):
            testList.extend(self.emptyBlock)
        with self.assertRaises(TypeError):
            testList.extend(ptypes.nil)
        
        with self.assertRaises(TypeError):
            testList.extend([42])
        with self.assertRaises(TypeError):
            testList.extend("xyz")
        with self.assertRaises(TypeError):
            testList.extend(range(5))
        with self.assertRaises(TypeError):
            testList.extend([ptypes.nil, 42])
        with self.assertRaises(TypeError):
            testList.extend(slice(3, 5))
        with self.assertRaises(TypeError):
            testList.extend(None)
    
    @unittest.skip("Index test TODO")
    def test_index(self):
        pass


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
    
    def test_bool(self):
        self.assertTrue(self.fourTen)
        self.assertTrue(self.fourNil)
        self.assertFalse(self.threeNegOne)
        self.assertTrue(self.nilFive)
    
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
    
    def test_index(self):
        with self.assertRaises(ValueError):
            self.fourTen.index(self.zero)
        self.assertEqual(self.fourTen.index(self.five), 1)
        with self.assertRaises(ValueError):
            self.fourTen.index(self.hundred)
        
        with self.assertRaises(ValueError):
            self.fourNil.index(self.zero)
        self.assertEqual(self.fourNil.index(self.five), 1)
        self.assertEqual(self.fourNil.index(self.hundred), 96)
        
        with self.assertRaises(ValueError):
            self.threeNegOne.index(self.zero)
        with self.assertRaises(ValueError):
            self.threeNegOne.index(self.five)
        
        self.assertEqual(self.nilFive.index(self.zero), 0)
        with self.assertRaises(ValueError):
            self.nilFive.index(self.five)
        with self.assertRaises(ValueError):
            print(self.nilFive.index(self.pi))
        
        with self.assertRaises(TypeError):
            self.fourNil.index(self.fourTen)
        with self.assertRaises(TypeError):
            self.fourTen.index(self.someList)
        with self.assertRaises(TypeError):
            self.fourTen.index(self.somePattern)
        with self.assertRaises(TypeError):
            self.fourTen.index(self.someBlock)
        with self.assertRaises(TypeError):
            self.fourNil.index(ptypes.nil)
        with self.assertRaises(TypeError):
            self.nilFive.index(ptypes.nil)


if __name__ == "__main__":
    unittest.main()
