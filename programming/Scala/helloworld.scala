object ConvertTime {

    def main(args: Array[String]) {
        val sc = new java.util.Scanner (System.in);
        var time = sc.next();
        var prefix = time.take(2).toInt;
        var sufix  = time.takeRight(2);

        if (sufix == "PM") {
            if (prefix < 12) {
                println(((prefix+12).toString) + ((time.drop(2)).take(6)));
            } else {
                println(time.take(8))
            }
        } else {
            if (prefix < 12) {
                println(time.take(8));
            } else {
                println("00" + ((time.drop(2)).take(6)));
            }
        }
    }
}
