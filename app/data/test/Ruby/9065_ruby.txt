module BrowserConwayGameOfLife
  class Cell
    NEIGHBOURS = [
      [-1, -1], [-1, 0], [-1, 1],
      [0, -1], [0, 1],
      [1, -1], [1, 0], [1, 1]
    ]

    attr_accessor :value

    def initialize(value = rand(0..1))
      (0..1).include?(value) ? @value = value : @value = rand(0..1)
    end

    def live?
      @value == 1
    end

    def dead?
      !live?
    end

    def change!
      self.value = self.value.send(:+, -1).abs
    end

    def to_s
      live? ? 'O' : ' '
    end

    def self.live_neighbours(universe, i, j)
      NEIGHBOURS.inject(0) do |sum, pos|
        sum + universe[(i + pos[0]) % universe.size][(j + pos[1]) % universe[0].size].value
      end
    end
  end
end
