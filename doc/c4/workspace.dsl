workspace "RRunning" "An R package for analyzing data from my Strava" {

    !identifiers hierarchical

    model {
        u = person "User"
        strava = softwareSystem "Strava"
        
        ss = softwareSystem "RRunning" {

          frontend = container "Front end"
          
          fs = container "Local file system" {
            tags "Database"
          }
          
          controller = container "Controller"

          local_data_controller = container "Local data controller"

          importer = container "Strava importer"

          rstrava = container "RStrava R package"

          importer -> rstrava "calls, receives data from"
          controller -> importer "invokes, receives data from"
          controller -> local_data_controller "sends data to, receives data from"
          local_data_controller -> fs "reads from, writes to"
          frontend -> controller "sends commands to, receives results from"
          
        }
        
        u -> ss.frontend "Interacts with"
        ss.rstrava -> strava "imports data from"
    }

    views {
        systemContext ss "Diagram1" {
            include *
            /* autolayout lr */
        }

        container ss "Diagram2" {
            include *
            /* autolayout lr */
        }

        styles {
            element "Element" {
                color #ffffff
            }
            element "Person" {
                background #ba1e75
                shape person
                fontSize 30
            }
            element "Software System" {
                background #d92389
                fontSize 30
            }
            element "Container" {
                background #f8289c
                fontSize 30
            }
            element "Database" {
                shape cylinder
                fontSize 30
            }
        }
    }

    configuration {
        scope softwaresystem
    }

}
