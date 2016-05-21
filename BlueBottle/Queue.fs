module Queue

type Queue<'a>() =
    let mutable list = []  
    member __.push (key: 'a) (value: int) =
        let rec push (list: ('a*int) list) =
            match list with
            | [] -> [key,value]
            | (k,v)::tail ->
                if value>=v then
                    (key,value)::(k,v)::tail
                else
                    (k,v)::(push tail)
        list<-(push list)
    member __.empty() =
        list.Length=0
    member __.pop() =
        match list with
        | [] -> failwith "Empty queue"
        | head::tail ->
            list<-tail
            head    

