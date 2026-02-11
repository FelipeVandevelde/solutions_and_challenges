##### Este é o Readme do projeto _em inglês_, caso prefira acompanhar o Readme em Português-BR, [clique aqui](./README-PT.md)
# 1000 - Two Sum

## [Descrição](https://leetcode.com/problems/two-sum/description/)

## Solution

To solve this problem I did the most basic things for now.
I created a brute force code, comparing each item and checking if the sum of that item with any other is equal to the desired number.

### Python 3.
```python
class Solution:
    def twoSum(self, nums: List[int], target: int) -> List[int]:
        for num1 in range(len(nums)):
            for num2 in range(len(nums)):
                if num1 != num2 and nums[num1] + nums[num2] == target:
                    return [num1, num2]
```