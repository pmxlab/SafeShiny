#' Interactive flame chart of tracked execution time (HTML/SVG)
#'
#' Same data and layout as \code{\link{PlotSafeShinyFlame}} (time on the x-axis, nesting depth
#' up the y-axis, fill by call type, outline for errors/silent stops), but as a self-contained
#' HTML widget with no external dependencies: hover a bar to see its label, type, status, duration,
#' start time and depth; scroll the mouse wheel to zoom around the cursor, drag to pan and
#' double-click to reset. Labels are drawn inside a bar only when they fit.
#'
#' The result is an \code{htmltools} tag: print it at the console to open it in the viewer/browser,
#' return it from \code{shiny::renderUI()}, or save it with \code{htmltools::save_html()}.
#'
#' @inheritParams PlotSafeShinyFlame
#' @param height numeric, height of one depth row in pixels (default \code{22}).
#'
#' @return an \code{htmltools} browsable tag, or \code{NULL} (invisibly, with a message) if nothing
#'   has been tracked yet.
#'
#' @examples
#' ResetSafeShinyTiming(session = NULL)
#' outer <- SafeShiny:::.startSafeShinyTiming(NULL, "observer", "observe")
#' inner <- SafeShiny:::.startSafeShinyTiming(NULL, "reactive", "react")
#' Sys.sleep(0.01)
#' SafeShiny:::.endSafeShinyTiming(NULL, inner, "ok")
#' SafeShiny:::.endSafeShinyTiming(NULL, outer, "ok")
#' w <- PlotSafeShinyFlameHTML(session = NULL)
#' class(w)
#'
#' @importFrom htmltools tags HTML tagList browsable
#' @importFrom jsonlite toJSON
#' @export
PlotSafeShinyFlameHTML <- function(session = shiny::getDefaultReactiveDomain(), height = 22) {
  raw <- GetSafeShinyTimingRaw(session = session)
  if (nrow(raw) == 0) {
    message("[SafeShiny] No tracked calls to plot.")
    return(invisible(NULL))
  }
  raw <- raw[order(raw$start), , drop = FALSE]
  t0 <- as.numeric(difftime(raw$start, min(raw$start), units = "secs"))
  top <- raw$depth == 0
  xmax <- max(t0 + raw$elapsed)
  untracked <- max(xmax - sum(raw$elapsed[top]), 0)

  dat <- data.frame(
    l = raw$label, y = ifelse(is.na(raw$type), "", raw$type), s = raw$status,
    t = t0, e = raw$elapsed, d = raw$depth, stringsAsFactors = FALSE
  )
  json <- as.character(jsonlite::toJSON(
    list(calls = dat, xmax = xmax, untracked = untracked, rowH = height),
    dataframe = "rows", auto_unbox = TRUE, digits = 6
  ))
  json <- gsub("</", "<\\/", json, fixed = TRUE)  # never let the data close the <script>
  id <- paste0("ssflame-", paste(sample(c(letters, 0:9), 10, replace = TRUE), collapse = ""))

  htmltools::browsable(htmltools::tagList(
    htmltools::tags$div(id = id, class = "ssflame"),
    htmltools::tags$script(htmltools::HTML(paste0(
      "(function(){var DATA=", json, ",ID=\"", id, "\";", .safeShinyFlameJS, "})();"
    )))
  ))
}

.safeShinyFlameJS <- '
var root=document.getElementById(ID);
var TYPE={observe:"#4C78A8",react:"#54A24B",render:"#B279A2",download:"#9D755D"};
var STATUS={silent:"#F2B134",error:"#E45756"};
var calls=DATA.calls,H=DATA.rowH,AX=26,maxD=0;
calls.forEach(function(c){if(c.d>maxD)maxD=c.d;});
var rows=Math.max(maxD+1,3),SVGH=rows*H+AX;
var v0=0,v1=DATA.xmax>0?DATA.xmax:1;
root.style.cssText="position:relative;font:12px sans-serif;color:#333";
var legend="";
Object.keys(TYPE).forEach(function(k){legend+="<span style=\\"margin-right:12px\\"><span style=\\"display:inline-block;width:10px;height:10px;background:"+TYPE[k]+";margin-right:4px\\"></span>"+k+"</span>";});
Object.keys(STATUS).forEach(function(k){legend+="<span style=\\"margin-right:12px\\"><span style=\\"display:inline-block;width:10px;height:10px;border:2px solid "+STATUS[k]+";margin-right:4px;box-sizing:border-box\\"></span>"+k+"</span>";});
root.innerHTML="<div style=\\"margin-bottom:4px\\">"+legend+"<span style=\\"float:right;color:#666\\">Untracked: "+DATA.untracked.toPrecision(3)+"s ("+Math.round(100*DATA.untracked/(DATA.xmax||1))+"% of "+DATA.xmax.toPrecision(3)+"s) &middot; wheel: zoom, drag: pan, double-click: reset</span></div><svg style=\\"width:100%;display:block;background:#fff;border:1px solid #ccc\\" height=\\""+SVGH+"\\"></svg><div class=\\"tip\\" style=\\"position:absolute;display:none;pointer-events:none;background:rgba(30,30,30,.92);color:#fff;padding:5px 8px;border-radius:3px;font-size:12px;white-space:nowrap;z-index:10\\"></div>";
var svg=root.querySelector("svg"),tip=root.querySelector(".tip"),NS="http://www.w3.org/2000/svg";
function W(){return svg.getBoundingClientRect().width||800;}
function esc(s){return String(s).replace(/[&<>"]/g,function(c){return {"&":"&amp;","<":"&lt;",">":"&gt;","\\"":"&quot;"}[c];});}
function draw(){
  var w=W(),sc=w/(v1-v0),out=[],i,c,x,bw,y,txt,maxCh;
  var nt=Math.max(2,Math.floor(w/110)),step=(v1-v0)/nt,mag=Math.pow(10,Math.floor(Math.log10(step)));
  step=[1,2,5,10].map(function(m){return m*mag;}).filter(function(s){return s>=step;})[0]||step;
  for(var t=Math.ceil(v0/step)*step;t<=v1;t+=step){
    x=(t-v0)*sc;
    out.push("<line x1=\\""+x+"\\" x2=\\""+x+"\\" y1=\\"0\\" y2=\\""+rows*H+"\\" stroke=\\"#eee\\"/><text x=\\""+x+"\\" y=\\""+(rows*H+16)+"\\" font-size=\\"11\\" text-anchor=\\"middle\\" fill=\\"#666\\">"+(+t.toPrecision(4))+"s</text>");
  }
  for(i=0;i<calls.length;i++){
    c=calls[i];
    if(c.t+c.e<v0||c.t>v1)continue;
    x=(c.t-v0)*sc;bw=c.e*sc;
    if(bw<0.4)continue;
    y=(rows-1-c.d)*H;
    var st=STATUS[c.s];
    out.push("<rect data-i=\\""+i+"\\" x=\\""+x+"\\" y=\\""+y+"\\" width=\\""+bw+"\\" height=\\""+(H-1)+"\\" fill=\\""+(TYPE[c.y]||"#999")+"\\" stroke=\\""+(st||"#fff")+"\\" stroke-width=\\""+(st?2:0.5)+"\\"/>");
    var vx=Math.max(x,0),vw=Math.min(x+bw,w)-vx;
    maxCh=Math.floor((vw-6)/6.4);
    if(maxCh>=3){
      txt=c.l.length>maxCh?c.l.slice(0,maxCh-1)+"\\u2026":c.l;
      out.push("<text x=\\""+(vx+3)+"\\" y=\\""+(y+H/2+4)+"\\" font-size=\\"11\\" fill=\\"#fff\\" pointer-events=\\"none\\">"+esc(txt)+"</text>");
    }
  }
  svg.innerHTML=out.join("");
}
function showTip(e){
  var r=e.target;
  if(!r.getAttribute||r.tagName!=="rect"||r.getAttribute("data-i")===null){tip.style.display="none";return;}
  var c=calls[+r.getAttribute("data-i")],b=root.getBoundingClientRect();
  tip.innerHTML="<b>"+esc(c.l)+"</b><br>type: "+(c.y||"?")+" &middot; status: "+c.s+"<br>duration: "+c.e.toPrecision(4)+"s<br>start: "+c.t.toPrecision(4)+"s &middot; depth: "+c.d;
  tip.style.display="block";
  var tx=e.clientX-b.left+12,ty=e.clientY-b.top+14;
  if(tx+tip.offsetWidth>b.width)tx=e.clientX-b.left-tip.offsetWidth-12;
  tip.style.left=Math.max(0,tx)+"px";tip.style.top=ty+"px";
}
var drag=null;
svg.addEventListener("mousemove",function(e){
  if(drag){var dx=(e.clientX-drag.x)*(drag.v1-drag.v0)/W(),span=drag.v1-drag.v0;v0=drag.v0-dx;v1=v0+span;draw();tip.style.display="none";return;}
  showTip(e);
});
svg.addEventListener("mouseleave",function(){tip.style.display="none";drag=null;});
svg.addEventListener("mousedown",function(e){drag={x:e.clientX,v0:v0,v1:v1};e.preventDefault();});
window.addEventListener("mouseup",function(){drag=null;});
svg.addEventListener("dblclick",function(){v0=0;v1=DATA.xmax>0?DATA.xmax:1;draw();});
svg.addEventListener("wheel",function(e){
  e.preventDefault();tip.style.display="none";
  var b=svg.getBoundingClientRect(),f=(e.clientX-b.left)/b.width,at=v0+f*(v1-v0),k=e.deltaY<0?0.8:1.25;
  var span=Math.min((v1-v0)*k,DATA.xmax>0?DATA.xmax:1);
  v0=at-f*span;v1=v0+span;
  if(v0<0){v1-=v0;v0=0;}
  draw();
},{passive:false});
window.addEventListener("resize",draw);
draw();
'
