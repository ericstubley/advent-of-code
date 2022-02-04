#! /usr/bin/env python3

import unittest
import puzzle_2016_04 as puz


class Test_2016_04(unittest.TestCase):
    def setUp(self):
        with open("test_input.txt") as f:
            data = f.readlines()

        self.rooms = puz.parse(data)


    def test_parse(self):
        test_room = puz.EncryptedRoom(name="aaaaa-bbb-z-y-x", sector=123, checksum="abxyz")
        self.assertEqual(self.rooms[0].name, test_room.name)
        self.assertEqual(self.rooms[0].sector, test_room.sector)
        self.assertEqual(self.rooms[0].checksum, test_room.checksum)


    def test_is_valid(self):
        self.assertTrue(puz.is_valid(self.rooms[0]))
        self.assertTrue(puz.is_valid(self.rooms[1]))
        self.assertTrue(puz.is_valid(self.rooms[2]))
        self.assertFalse(puz.is_valid(self.rooms[3]))


    def test_valid_sector_id_sum(self):
        self.assertEqual(puz.valid_sector_id_sum(self.rooms), 1514)


    def test_decrypt_name(self):
        test_room = puz.EncryptedRoom(name="qzmt-zixmtkozy-ivhz", sector=343, checksum="aaaaa")
        self.assertEqual(puz.decrypt_name(test_room), "very encrypted name")


if __name__ == "__main__":
    unittest.main()
