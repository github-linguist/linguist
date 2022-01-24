# @version ^0.2
from vyper.interfaces import ERC20

interface UniswapV2Router:
  def swapExactTokensForTokens(
    amountIn: uint256,
    amountOutMin: uint256,
    path: address[3],
    to: address,
    deadline: uint256
  ) -> uint256[3]: nonpayable

UNISWAP: constant(address) = 0x7a250d5630B4cF539739dF2C5dAcb4c659F2488D
WETH: constant(address) = 0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2

@external
def swap(tokenIn: address, tokenOut: address, amountIn: uint256):
  ERC20(tokenIn).transferFrom(msg.sender, self, amountIn)
  ERC20(tokenIn).approve(UNISWAP, amountIn)

  # pseudo code
  # path: address[]
  # path[0] = tokenIn
  # path[1] = WETH
  # path[2] = tokenOut

  # UniswapV2Router(UNISWAP).swapExactTokensForTokens(amount, 0, path, self, block.timestamp)

  res: Bytes[128] = raw_call(
    UNISWAP,
    concat(
      method_id("swapExactTokensForTokens(uint256,uint256,address[],address,uint256)"),
      convert(amountIn, bytes32),        # amount in
      convert(0, bytes32),               # amount out min
      convert(160, bytes32),             # path[] offset (5 * 32, 5 = number of func args)
      convert(self, bytes32),            # to
      convert(block.timestamp, bytes32), # deadline
      convert(3, bytes32),               # path[] length
      convert(tokenIn, bytes32),
      convert(WETH, bytes32),
      convert(tokenOut, bytes32)
    ),
    max_outsize=128,
  )
