workspace "RunneR" "An R package for analyzing data from my Strava" {

    !identifiers hierarchical
    !adrs adr
    !docs documentation

    model {
        u = person "User"
        strava = softwareSystem "Strava"
        
        ss = softwareSystem "RunneR System" {

          fs = container "Local file system" {
            tags "Database"
          }
          
          rstrava = container "RStrava R package"
          
          runner = container "RunneR" {
            front_end = component "Front end"
            main_controller = component "Main controller"
            local_data_controller = component "Local data controller"
            importer = component "Strava importer"

            importer -> rstrava "calls, receives data from"
            main_controller -> importer "invokes, receives data from"
            main_controller -> local_data_controller "sends data to, receives data from"
            local_data_controller -> fs "reads from, writes to"
            front_end -> main_controller "sends commands to, receives results from"
            
          }
          
        }
        
        u -> ss.runner.front_end "Interacts with"
        ss.rstrava -> strava "imports data from"

    }

    views {
        systemContext ss "system_context" {
            include *
            /* autolayout lr */
        }

        container ss "container_diagram" {
            include *
            /* autolayout lr */
        }

        component ss.runner "component_diagram" {
          include *
        }

        styles {
            element "Element" {
                color #ffffff
            }
            element "Person" {
                background #ca2e85
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
            element "Component" {
                background #d8087c
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
