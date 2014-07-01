MOAISim.openWindow('Path', 1280, 720)

local viewport = MOAIViewport.new()
viewport:setScale(100, 0)
viewport:setSize(1280, 720)

local layer = MOAILayer2D.new()
layer:setViewport(viewport)
layer:showDebugLines(true)
MOAISim.pushRenderPass(layer)

local camera = MOAICamera2D.new()
camera:setLoc(0.0, 0.0)
layer:setCamera(camera)

local world = MOAIBox2DWorld.new()
layer:setBox2DWorld(world)

local body = world:addBody(MOAIBox2DBody.STATIC)
local loader, err = loadfile('path.lua')
if not loader then
  print(err)
else
  for layerName, layer in pairs(loader()) do
    for iObject, object in pairs(layer) do
      local poly = object.poly
      --if poly then body:addChain(poly, true) end
      --for iCoord = 1, #poly / 2 do
      --  body:addCircle(poly[iCoord * 2 - 1], poly[iCoord * 2], 0.2)
      --end
      if object.convex then
        for _,poly in pairs(object.convex) do
          body:addChain(poly, true)
          for iCoord = 1, #poly / 2 do
            body:addCircle(poly[iCoord * 2 - 1], poly[iCoord * 2], 0.15)
          end
        end
      end
    end
  end
  world:start()
end
