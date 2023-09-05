def left_and_right(x):

    left  = []
    right = []

    for i in range(0, len(x), 2):
        left.append(x[i])
        right.append(x[i + 1])

    return (left, right)



def compare(left, right):

    print(f"Compare {left} vs {right}")

    if type(left) == int and type(right) == int:
        return left <= right
    elif type(left) == list and type(right) == list:
        '''
        purity = True

        length = min(len(left), len(right))

        if len(left) == 0 and len(right) == 0:
            return 0

        for i in range(length):
            purity = purity and compare(left[i], right[i])

        return purity
        '''

        length = len(left)

        if len(left) > len(right):
            return False
        
        for i in range(len(left)):
            if not compare(left[i], right[i]):
                return False

        return True
    else:
        if type(left) == int and type(right) == list:
            return len(right) != 0 or left < right[0]
        else:
            return compare(left, [right])

with open("Input.txt", 'r') as f:
    contents = list(map(lambda x : (eval(x)), list(filter(lambda x : (x != ''), f.read().split('\n')))))
    (left_side, right_side) = left_and_right(contents)

    assert(len(left_side) == len(right_side))
    
    n = 0


    for i in range(len(left_side)):
        if compare(left_side[i], right_side[i]):
            n += (i + 1)
            print(f"{i + 1} is cool\n")
        else:
            print(f"{i + 1} is NOT cool\n")

    print(n)

# 2874 too low
# 2908 too low
# 3309 too low
