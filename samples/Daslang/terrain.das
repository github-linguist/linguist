require math

struct HeightMap {
  width: int
  height: int
  data: array<float>
}

def simple_noise(x, y: float): float {
  let ix = int(x)
  let iy = int(y)
  let fx = x - float(ix)
  let fy = y - float(iy)
  
  let hash = (ix * 73856093) ^ (iy * 19349663)
  let noise = float(hash % 1000) / 500.0f - 1.0f
  
  return noise * fx * fy
}

def generate_terrain(width, height: int) {
  var heightmap = HeightMap(width = width, height = height)
  heightmap.data |> resize(width * height)
  
  for (y in range(height)) {
    for (x in range(width)) {
      let fx = float(x) * 0.1f
      let fy = float(y) * 0.1f
      
      var height_value = 0.0f
      height_value += simple_noise(fx, fy) * 50.0f
      height_value += simple_noise(fx * 2.0f, fy * 2.0f) * 25.0f
      height_value += simple_noise(fx * 4.0f, fy * 4.0f) * 12.5f
      
      heightmap.data[y * width + x] = height_value
    }
  }
  
  return <- heightmap
}

def smooth_terrain(var heightmap: HeightMap) {
  var temp_data: array<float>
  temp_data |> resize(length(heightmap.data))
  
  for (y in range(1, heightmap.height - 1)) {
    for (x in range(1, heightmap.width - 1)) {
      let idx = y * heightmap.width + x
      var sum = 0.0f
      
      for (dy in range(-1, 2)) {
        for (dx in range(-1, 2)) {
          let neighbor_idx = (y + dy) * heightmap.width + (x + dx)
          sum += heightmap.data[neighbor_idx]
        }
      }
      
      temp_data[idx] = sum / 9.0f
    }
  }
  
  for (i in range(length(heightmap.data))) {
    heightmap.data[i] = temp_data[i]
  }
}

def get_height(heightmap: HeightMap; x, y: int): float {
  if (x >= 0 && x < heightmap.width && y >= 0 && y < heightmap.height) {
    return heightmap.data[y * heightmap.width + x]
  }
  return 0.0f
}

def find_spawn_location(heightmap: HeightMap): int2 {
  for (y in range(heightmap.height)) {
    for (x in range(heightmap.width)) {
      let h = get_height(heightmap, x, y)
      if (h > 5.0f && h < 30.0f) {
        return int2(x, y)
      }
    }
  }
  
  return int2(heightmap.width / 2, heightmap.height / 2)
}

def calculate_slope(heightmap: HeightMap; x, y: int): float {
  let center = get_height(heightmap, x, y)
  let right = get_height(heightmap, x + 1, y)
  let down = get_height(heightmap, x, y + 1)
  
  let dx = right - center
  let dy = down - center
  
  return sqrt(dx * dx + dy * dy)
}

def print_terrain_section(heightmap: HeightMap; start_x, start_y, size: int) {
  print("Terrain heights:\n")
  for (y in range(start_y, start_y + size)) {
    var line = ""
    for (x in range(start_x, start_x + size)) {
      let h = get_height(heightmap, x, y)
      if (h < 0.0f) {
        line += "~ "
      } elif (h < 10.0f) {
        line += ". "
      } elif (h < 30.0f) {
        line += "^ "
      } else {
        line += "# "
      }
    }
    print(line)
  }
}

[export]
def main() {
  print("Generating 32x32 terrain\n")
  
  var terrain <- generate_terrain(32, 32)
  
  print("Smoothing terrain...\n")
  smooth_terrain(terrain)
  
  let spawn = find_spawn_location(terrain)
  let spawn_height = get_height(terrain, spawn.x, spawn.y)
  print("Spawn location: {spawn.x}, {spawn.y} at height {spawn_height}\n")
  
  print_terrain_section(terrain, 10, 10, 12)
  
  print("\nSlope analysis around spawn:\n")
  for (dy in range(-2, 3)) {
    for (dx in range(-2, 3)) {
      let x = spawn.x + dx
      let y = spawn.y + dy
      let slope = calculate_slope(terrain, x, y)
      if (slope > 15.0f) {
        print("Steep slope at {x}, {y}: {slope}\n")
      }
    }
  }
}