test_that("clip_vector handles intersecting geometries", {
  vector <- sf::st_as_sf(data.frame(
    id = 1:3,
    geometry = sf::st_sfc(sf::st_point(c(0.5, 0.5)), sf::st_point(c(1.5, 1.5)), sf::st_point(c(2.5, 2.5)))
  ), crs = 4326)

  polygon <- sf::st_as_sf(data.frame(id = 1, geometry = sf::st_sfc(st_polygon(list(
    rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))
  )))), crs = 4326)

  result <- clip_vector(vector, polygon)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
})

test_that("clip_vector handles disjoint geometries", {
  vector <- sf::st_as_sf(data.frame(
    id = 1:3,
    geometry = sf::st_sfc(sf::st_point(c(10, 10)), sf::st_point(c(11, 11)), sf::st_point(c(12, 12)))
  ), crs = 4326)

  polygon <- sf::st_as_sf(data.frame(
    id = 1,
    geometry = sf::st_sfc(sf::st_polygon(list(
      rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))
    )))
  ), crs = 4326)

  result <- clip_vector(vector, polygon)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 0)
})

test_that("clip_vector handles CRS transformations", {
  vector <- sf::st_as_sf(data.frame(
    id = 1:3,
    geometry = sf::st_sfc(sf::st_point(c(0.5, 0.5)), sf::st_point(c(1.5, 1.5)), sf::st_point(c(2.5, 2.5)))
  ), crs = 4326)

  polygon <- sf::st_as_sf(data.frame(
    id = 1,
    geometry = sf::st_sfc(sf::st_polygon(list(
      rbind(c(0, 0), c(200000, 0), c(200000, 200000), c(0, 200000), c(0, 0))
    )))
  ), crs = 32630)

  result <- clip_vector(vector, polygon)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
  expect_equal(sf::st_crs(result), sf::st_crs(polygon))
})


test_that("safe_clip_multipolygon handles correct clipping", {
  vector <- sf::st_as_sf(data.frame(
    id = 1:2,
    geometry = sf::st_sfc(sf::st_polygon(list(
      rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))
    )), sf::st_polygon(list(
      rbind(c(1, 1), c(3, 1), c(3, 3), c(1, 3), c(1, 1))
    )))
  ), crs = 4326)

  polygon <- sf::st_as_sf(data.frame(
    id = 1,
    geometry = sf::st_sfc(sf::st_polygon(list(
      rbind(c(1, 1), c(2, 1), c(2, 2), c(1, 2), c(1, 1))
    )))
  ), crs = 4326)

  result <- safe_clip_multipolygon(vector, polygon)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
})

test_that("safe_clip_multipolygon handles invalid MULTIPOLYGON encoding",
          {
            vector <- sf::st_as_sf(data.frame(
              id = 1:2,
              geometry = sf::st_sfc(
                sf::st_geometrycollection(list(sf::st_polygon(list(
                  rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))
                )))),
                sf::st_geometrycollection(list(sf::st_polygon(list(
                  rbind(c(1, 1), c(3, 1), c(3, 3), c(1, 3), c(1, 1))
                ))))
              )
            ), crs = 4326)

            polygon <- sf::st_as_sf(data.frame(
              id = 1,
              geometry = sf::st_sfc(sf::st_polygon(list(
                rbind(c(1, 1), c(2, 1), c(2, 2), c(1, 2), c(1, 1))
              )))
            ), crs = 4326)

            result <- safe_clip_multipolygon(vector, polygon)

            expect_s3_class(result, "sf")
            expect_equal(nrow(result), 1)
          })

test_that("safe_clip_multipolygon handles CRS transformations", {
  vector <- sf::st_as_sf(data.frame(
    id = 1:2,
    geometry = sf::st_sfc(sf::st_polygon(list(
      rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))
    )), sf::st_polygon(list(
      rbind(c(1, 1), c(3, 1), c(3, 3), c(1, 3), c(1, 1))
    )))
  ), crs = 4326)

  polygon <- sf::st_as_sf(data.frame(
    id = 1,
    geometry = sf::st_sfc(sf::st_polygon(list(
      rbind(
        c(100000, 100000),
        c(200000, 100000),
        c(200000, 200000),
        c(100000, 200000),
        c(100000, 100000)
      )
    )))
  ), crs = 32630)

  result <- safe_clip_multipolygon(vector, polygon)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 0)
  expect_equal(sf::st_crs(result), sf::st_crs(polygon))
})

test_that("safe_clip_multipolygon returns empty for disjoint geometries",
          {
            vector <- sf::st_as_sf(data.frame(
              id = 1:2,
              geometry = sf::st_sfc(sf::st_polygon(list(
                rbind(c(10, 10), c(12, 10), c(12, 12), c(10, 12), c(10, 10))
              )), sf::st_polygon(list(
                rbind(c(15, 15), c(18, 15), c(18, 18), c(15, 18), c(15, 15))
              )))
            ), crs = 4326)

            polygon <- sf::st_as_sf(data.frame(
              id = 1,
              geometry = sf::st_sfc(sf::st_polygon(list(
                rbind(c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0))
              )))
            ), crs = 4326)

            result <- safe_clip_multipolygon(vector, polygon)

            expect_s3_class(result, "sf")
            expect_equal(nrow(result), 0)
          })
