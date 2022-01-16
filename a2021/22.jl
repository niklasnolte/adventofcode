import DataStructures:OrderedDict

Point = Tuple{Int64, Int64, Int64}
Cuboid = Tuple{Point, Point}
Instruction = Tuple{Bool, Cuboid}
Grid = Vector{Cuboid}

function parseInstr(inp::String)::Instruction
  onoff, cuboid = split(inp)
  onoff = onoff == "on"
  l = Vector{Int64}()
  r = Vector{Int64}()
  for xi in split(cuboid, ",")
    li, ri = split(xi[3:end], "..")
    push!(l, parse(Int64, li))
    push!(r, parse(Int64, ri))
  end
  return onoff, (Tuple(l), Tuple(r))
end

getInput() = readlines("input22.txt") .|> parseInstr

function part1(instrs::Vector{Instruction})
  grid = Set{Point}()
  for (on, (l,r)) in instrs
    lx,ly,lz = l
    rx,ry,rz = r
    if (lx, ly, lz) .|> <(-50) |> any
      continue
    elseif (lx, ry, rz) .|> >(50) |> any
      continue
    end
    for x in lx:rx
      for y in ly:ry
        for z in lz:rz
          if on
            push!(grid, (x,y,z))
          else
            delete!(grid, (x,y,z))
          end
        end
      end
    end
  end
  length(grid) |> println
end

function encodeSparse(instrs::Vector{Instruction})
  Xs = Vector{Int64}()
  Ys = Vector{Int64}()
  Zs = Vector{Int64}()
  for (_, (l,r)) in instrs
    lx,ly,lz = l
    rx,ry,rz = r
    push!(Xs, lx, rx+1)
    push!(Ys, ly, ry+1)
    push!(Zs, lz, rz+1)
  end
  return (Xs, Ys, Zs) .|>
            unique .|>
            sort .|>
            XI -> OrderedDict(x => i for (i,x) in enumerate(XI))
end


function part2(instrs::Vector{Instruction})
  Xs, Ys, Zs = encodeSparse(instrs)
  kXs, kYs, kZs = (Xs, Ys, Zs) .|> XI -> [ x for x in keys(XI) ]
  grid = Set{Point}()
  for (on, (l,r)) in instrs
    lx,ly,lz = l
    rx,ry,rz = r .+ 1
    for ix in Xs[lx]:Xs[rx]-1
      for iy in Ys[ly]:Ys[ry]-1
        for iz in Zs[lz]:Zs[rz]-1
          if on
            push!(grid, (ix, iy, iz))
          else
            delete!(grid, (ix, iy, iz))
          end
        end
      end
    end
  end

  n_ons = 0  
  # calculate overall volume
  for (ix,iy,iz) in grid
    lx = kXs[ix+1] - kXs[ix]
    ly = kYs[iy+1] - kYs[iy]
    lz = kZs[iz+1] - kZs[iz]
    n_ons += lx * ly * lz
  end
  println(n_ons)
end

function main()
  instrs = getInput()
  part1(instrs)
  part2(instrs)
end

main()
