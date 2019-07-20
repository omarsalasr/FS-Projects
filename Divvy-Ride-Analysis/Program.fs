//
// F# program to analyze Divvy daily ride data.
//
// << Omar Salas-Rodriguez >>
// U. of Illinois, Chicago
// CS 341, Spring 2019
// Project #04
//

#light

module Project04

//
// ParseLine and ParseInput
//
// Given a sequence of strings representing Divvy data, 
// parses the strings and returns a list of lists.  Each
// sub-list denotes one bike ride.  Example:
//
//   [ [176;74;1252;21;595;1986;1]; ... ]
//
// The values are station id (from), station id (to), bike
// id, starting hour (0..23), trip duration (secs), birth
// year (0=>not specified), and gender (0=>not specified, 
// 1=>identifies as male, 2=>identifies as female).
//
let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints

let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides

// Print n number of stars
let rec printStars n =
  match n with
  | 0 -> ()
  | 1 -> printf "*"
  | _ -> printf "*"
         printStars (n-1)

// Get the Nth element from a list
let rec getElm L n =
  match L with
  | [] -> -1
  | hd::_ when n = 0 -> hd
  | _::tl -> getElm tl (n-1)

// Get the Nth list element from a list of lists
let rec getNthMember L n =
  match L with
  | [] -> []
  | hd::_ when n = 0 -> hd
  | _::tl -> getNthMember tl (n-1)

// Get the number of riders who identify as male
let rec getMaleRiders L =
  match L with
  | [] -> 0
  | hd::tl when (getElm hd 6) = 1 -> 1 + getMaleRiders tl
  | _::tl -> 0 + getMaleRiders tl

// Get the number of riders who identify as female
let rec getFemaleRiders L =
  match L with
  | [] -> 0
  | hd::tl when (getElm hd 6) = 2 -> 1 + getFemaleRiders tl
  | _::tl -> 0 + getFemaleRiders tl

// Get the sum of the ages for all the riders
let rec getTotalAge L currYear =
  match L with
  | [] -> 0
  | hd::tl when (getElm hd 5) <> 0 -> currYear - (getElm hd 5) + getTotalAge tl currYear
  | _:: tl -> 0 + getTotalAge tl currYear

// Get the total riders with valid age value
let rec getAgeSize L =
  match L with 
  | [] -> 0
  | hd::tl when (getElm hd 5) <> 0 -> 1 + getAgeSize tl
  | _::tl -> 0 + getAgeSize tl

// Get the total number of riders in a range of time duration
let rec getDuration L lower upper =
  match L with
  | [] -> 0
  | hd::tl when ((getElm hd 4) / 60) >= lower && ((getElm hd 4) / 60) < upper -> 1 + getDuration tl lower upper
  | _::tl -> 0 + getDuration tl lower upper

// Helper function that returns the number of riders at a given start time
let rec _printHistogram L n =
  match L with
  | [] -> 0
  | hd::tl when (getElm hd 3) = n -> 1 + _printHistogram tl n
  | _::tl -> 0 + _printHistogram tl n

// Print the histogram for all the riders starting at a given time
let rec printHistogram L n =
  match n with
  | 24 -> ()
  | _ -> let riders = _printHistogram L n
         printf " %A: " n
         printStars (riders/10)
         printfn "%A" riders
         printHistogram L (n+1)



[<EntryPoint>]
let main argv =
  //
  // input file name, then input divvy ride data and build
  // a list of lists:
  //
  printf "filename> "
  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents

  // printfn "%A" ridedata
  let N = List.length ridedata
  printfn ""
  printfn "# of riders: %A" N
  printfn ""

  let maleRiders = getMaleRiders ridedata
  printfn "%% of riders identifying as male: %A (%A%%)" maleRiders (((float maleRiders)/(float N))*100.0)
  
  let femaleRiders = getFemaleRiders ridedata
  printfn "%% of riders identifying as female: %A (%A%%)" femaleRiders (((float femaleRiders)/(float N))*100.0)
  printfn ""

  let currYear = System.DateTime.Now.Year
  let totalAge = getTotalAge ridedata currYear
  let totalAgeSize = getAgeSize ridedata
  if totalAgeSize = 0 then
    printfn "Average age: 0.0"
  else 
    printfn "Average age: %A" ((float totalAge)/(float totalAgeSize))
  printfn ""


  printfn "** Ride Durations:"
  let dur0to30 = getDuration ridedata 0 30
  printfn " 0..30 mins: %A (%A%%)" dur0to30 (((float dur0to30)/(float N))*100.0)

  let dur30to60 = getDuration ridedata 30 60
  printfn " 30..60 mins: %A (%A%%)" dur30to60 (((float dur30to60)/(float N))*100.0)

  let dur60to120 = getDuration ridedata 60 120
  printfn " 60..120 mins: %A (%A%%)" dur60to120 (((float dur60to120)/(float N))*100.0)

  let dur120 = getDuration ridedata 120 1440
  printfn " > 2 hours: %A (%A%%)" dur120 (((float dur120)/(float N))*100.0)
  printfn ""

  printfn "** Ride Start Time Histogram:"
  printHistogram ridedata 0

  0 
