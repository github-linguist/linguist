<?hh // strict

type DBResultExtra = shape('age' => int);
type DBResult = shape(
  'id' => int,
  'name' => string,
  'extra' => DBResultExtra,
);

final class FakeDB {
  public function getRawRows(): array<array<string, mixed>> {
    $good_extra = json_encode(array('age' => 40));
    $bad_extra = 'corrupt data';
    // Real code would query a DB, but for now let's hardcode it
    return array(
      array(
        'id' => 123,
        'name' => 'Alice',
        'extra' => $good_extra,
      ),
      array(
        'id' => 456,
        'name' => 'Bob',
        'extra' => $bad_extra,
      ),
    );
  }

  /**
   * When processing untyped data you need to check each piece of data and
   * figure out whether to give up or recover when the data is bad
   */
  public function processRow(array<string, mixed> $row): ?DBResult {
    $row = Map::fromArray($row);
    $id = $row->contains('id') ? $row['id'] : null;
    $name = $row->contains('name') ? $row['name'] : null;
    $extra = $row->contains('extra') ? json_decode($row['extra'], true) : null;

    // Ignore rows with invalid IDs or names
    if (!is_int($id) || !is_string($name)) {
      return null;
    }

    // Try to recover from a bad extra column
    if (!is_array($extra)) {
      $extra = shape('age' => 0);
    } else {
      $extra = Map::fromArray($extra);
      $extra = shape('age' => $extra->contains('age') ? $extra['age'] : 0);
    }

    return shape('id' => $id, 'name' => $name, 'extra' => $extra);
  }

  public function getDBResults(): Vector<DBResult> {
    $ret = Vector {};
    foreach ($this->getRawRows() as $raw_row) {
      $row = $this->processRow($raw_row);
      if ($row !== null) {
        $ret->add($row);
      }
    }
    return $ret;
  }
}
