<?php

function forwardDiff($anArray, $times = 1) {
  if ($times <= 0) { return $anArray; }
  for ($accumilation = array(), $i = 1, $j = count($anArray); $i < $j; ++$i) {
    $accumilation[] = $anArray[$i] - $anArray[$i - 1];
  }
  if ($times === 1) { return $accumilation; }
  return forwardDiff($accumilation, $times - 1);
}

class ForwardDiffExample extends PweExample {

  function _should_run_empty_array_for_single_elem() {
    $expected = array($this->rand()->int());
    $this->spec(forwardDiff($expected))->shouldEqual(array());
  }

  function _should_give_diff_of_two_elem_as_single_elem() {
    $twoNums = array($this->rand()->int(), $this->rand()->int());
    $expected = array($twoNums[1] - $twoNums[0]);
    $this->spec(forwardDiff($twoNums))->shouldEqual($expected);
  }

  function _should_compute_correct_forward_diff_for_longer_arrays() {
    $diffInput = array(10, 2, 9, 6, 5);
    $expected  = array(-8, 7, -3, -1);
    $this->spec(forwardDiff($diffInput))->shouldEqual($expected);
  }

  function _should_apply_more_than_once_if_specified() {
    $diffInput = array(4, 6, 9, 3, 4);
    $expectedAfter1 = array(2, 3, -6, 1);
    $expectedAfter2 = array(1, -9, 7);
    $this->spec(forwardDiff($diffInput, 1))->shouldEqual($expectedAfter1);
    $this->spec(forwardDiff($diffInput, 2))->shouldEqual($expectedAfter2);
  }

  function _should_return_array_unaltered_if_no_times() {
    $this->spec(forwardDiff($expected = array(1,2,3), 0))->shouldEqual($expected);
  }

}
