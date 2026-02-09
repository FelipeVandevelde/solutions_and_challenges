# 1000 - Two Sum

## [Descrição](https://leetcode.com/problems/two-sum/description/)

## Solução



### Python 3
```python
class Solution:
    def twoSum(self, nums: List[int], target: int) -> List[int]:
        for num1 in range(len(nums)):
            for num2 in range(len(nums)):
                if num1 != num2 and nums[num1] + nums[num2] == target:
                    return [num1, num2]
```