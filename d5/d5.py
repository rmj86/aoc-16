import md5
import itertools

def passwordGenerator(key):
    i = 0
    base = md5.new(key)
    while True:
        enum = base.copy()
        enum.update(str(i))
        hash = enum.hexdigest()
        if hash.startswith("00000"):
            yield hash
        i += 1
    
def part1(key):
    pwgen = passwordGenerator(key)
    password = ""
    while len(password) < 8:
        hash = pwgen.next()
        password += hash[5]
    return password
    
def part2(key):
    pwgen = passwordGenerator(key)
    chars = [None]*8
    while None in chars:
        hash = pwgen.next()
        pos = int( hash[5], 16 )
        if pos < 8 and chars[pos] is None:
            chars[pos] = hash[6]
    password = ""
    for c in chars:
        password += c
    return password
    
def main():
    with open("input") as f:
        key = f.readline().strip()
    print "Advent of Code - day 5"
    print "  Part 1:\n   ", part1(key)
    print "  Part 2:\n   ", part2(key)
    
main()
