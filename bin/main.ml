let make_raster fname dimensions gt wkt =
  let driver = Gdal.Driver.get_by_name_exn "GTiff" in
  let raster =
    Gdal.Data_set.create_exn
      ~options:["COMPRESS=LZW"]
      ~bands:(1, Gdal.Band.Data.Byte) driver fname dimensions
  in
  Gdal.Data_set.set_projection raster wkt;
  Gdal.Geo_transform.set raster gt;
  raster

let process species season = 
  let filename = Printf.sprintf "/maps/mwd24/tmp/iucn/%s_%s.tif" species season in

  let gt, wkt = Gdal.Data_set.with_source_exn filename (fun ds ->
    Printf.printf "%dx%d\n" (Gdal.Data_set.get_x_size ds) (Gdal.Data_set.get_y_size ds);
    let band = Gdal.Data_set.get_band ds 1 Gdal.Band.Data.Byte in
    let sum = Gdal.Band.fold band (fun _x _y v a ->
      a + v
    ) 0 in
    Printf.printf "total: %d\n" sum;
    (Gdal.Geo_transform.get ds, Gdal.Data_set.get_projection ds)
  ) in
  let pixel_size = Gdal.Geo_transform.get_pixel_size gt in

  Gdal.Data_source.with_source_exn "/maps/mwd24/iucn-aoh/test_species_hab_elev.geojson" (fun ds ->
    let layers = Gdal.Data_source.get_layer_count ds in
    Printf.printf "layers %d\n" layers;

    let layer = Gdal.Data_source.get_layer ds 0 in
    Printf.printf "Layer %s\n" (Gdal.Layer.get_name layer);

    let filter = Printf.sprintf "id_no = %s AND seasonal = %s" species season in
    Gdal.Layer.set_attribute_filter layer filter;
    (* Gdal.Layer.reset_reading layer; *)
    let extent = Gdal.Layer.get_extent layer 1 in

    let rxscale, ryscale = pixel_size in
    let xscale = Float.abs rxscale
    and yscale = Float.abs ryscale in
    let min_x, max_y, max_x, min_y = (
      ((Float.floor (extent.min_x /. xscale)) *. xscale),
      ((Float.ceil (extent.max_y  /. yscale)) *. yscale),
      ((Float.ceil (extent.max_x /. xscale)) *. xscale),
      ((Float.floor (extent.min_y  /. yscale)) *. yscale)
    ) in

    let dimensions = (
      Int.of_float (Float.ceil((max_x -. min_x) /. xscale)),
      Int.of_float (Float.ceil((max_y -. min_y) /. yscale))
    ) in

    let ngt = Gdal.Geo_transform.make 
      ~origin:(min_x, max_y)
      ~pixel_size:pixel_size
      ~rotation:(0., 0.)
    in
    Printf.printf "origin: %f %f\n" min_x max_y;
    Printf.printf "scale: %f %f\n" rxscale ryscale;
    Printf.printf "projection: %s\n" wkt;
    let w, h = dimensions in
    Printf.printf "size: %d %d\n" w h;

    let fname = Printf.sprintf "/tmp/range_%s_%s.tif" species season in
    let raster = make_raster fname dimensions ngt wkt in
    Gdal.Alg.rasterize_layers ~options:["ALL_TOUCHED=TRUE"] raster [1] [(layer, [1.0])];
    (* match Gdal.Layer.get_feature layer 1 with
    | None -> Printf.printf "no feature"
    | Some feat -> 
    let geom = Gdal.Feature.get_geometry_ref feat in
    Gdal.Alg.rasterize_geometries raster [1] [(geom, [1.])] *)
  )

exception Invalid_file_row

let () =  
  Gdal.Lib.init_dynamic ();
  Gdal.Lib.register_all ();

  Csv.load "batch.csv" |> List.iter (fun row ->
    match row with
    | species_id :: season :: [] -> process species_id season
    | _ -> raise Invalid_file_row
  )
    