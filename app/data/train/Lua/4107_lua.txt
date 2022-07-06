res_node_types = {}

function res.add_node_type(id,name)
	table.insert(res_node_types,{id,name})
	res.log("verbose","Added node class \""..name.."\" with id: "..id)
end

function res.add_node(type,material,tiles,groups,drop,sounds)

	local id = "res:"..type.."_"..material
	local properties = {
		description = res.get_material_name(material).." "..res.get_nodetype_name(type),
		tiles = tiles,
		groups = groups,
		drop = drop,
		sounds = sounds,
	}

	res.add_item("node",id,properties)
end
