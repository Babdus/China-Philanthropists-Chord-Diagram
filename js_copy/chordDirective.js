angular.module('app').directive('chordDiagram', ['$window', 'matrixFactory',

  function ($window, matrixFactory) {

    var link = function ($scope, $el, $attr) {

      var size = [750, 750]; // SVG SIZE WIDTH, HEIGHT
      var marg = [50, 50, 50, 50]; // TOP, RIGHT, BOTTOM, LEFT
      var dims = []; // USABLE DIMENSIONS
      dims[0] = size[0] - marg[1] - marg[3]; // WIDTH
      dims[1] = size[1] - marg[0] - marg[2]; // HEIGHT

      var colors = d3.scale.ordinal()
          .range(["#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2", "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F" ]);

      var chord = d3.layout.chord()
          .padding(0.02)
          .sortGroups(d3.descending)
          .sortSubgroups(d3.ascending);

      var matrix = matrixFactory.chordMatrix()
          .layout(chord)
          .filter(function (item, r, c) {
            return (item.importer1 === r.name && item.importer2 === c.name) ||
                (item.importer1 === c.name && item.importer2 === r.name);
          })
          .reduce(function (items, r, c) {
            var value;
            if (!items[0]) {
              value = 0;
            } else {
              value = items.reduce(function (m, n) {
                if (r === c) {
                  return m + (n.flow1 + n.flow2);
                } else {
                  return m + (n.importer1 === r.name ? n.flow1: n.flow2);
                }
              }, 0);
            }
            return {value: value, data: items};
          });

      var innerRadius = (dims[1] / 2) - 100;

      var arc = d3.svg.arc()
          .innerRadius(innerRadius)
          .outerRadius(innerRadius + 20);

      var path = d3.svg.chord()
          .radius(innerRadius);

      var svg = d3.select($el[0]).append("svg")
          .attr("class", "chart")
          .attr({width: size[0] + "px", height: size[1] + "px"})
          .attr("preserveAspectRatio", "xMinYMin")
          .attr("viewBox", "0 0 " + size[0] + " " + size[1]);

      var container = svg.append("g")
          .attr("class", "container")
          .attr("transform", "translate(" + ((dims[0] / 2) + marg[3]) + "," + ((dims[1] / 2) + marg[0]) + ")");

      var messages = svg.append("text")
          .attr("class", "messages")
          .attr("transform", "translate(10, 10)")
          .text("Updating...");

      $scope.drawChords = function (data) {

        messages.attr("opacity", 1);
        messages.transition().duration(1000).attr("opacity", 0);

        matrix.data(data)
            .resetKeys()
            .addKeys(['importer1', 'importer2'])
            .update()

        var groups = container.selectAll("g.group")
            .data(matrix.groups(), function (d) { return d._id; });

        var gEnter = groups.enter()
            .append("g")
            .attr("class", "group");

        gEnter.append("path")
            .style("pointer-events", "none")
            .style("fill", function (d) { return colors(d._id); })
            .attr("d", arc);

        gEnter.append("text")
            .attr("dy", ".35em")
            .style("fill", "white")
            .on("click", groupClick)
            .on("mouseover", dimChords)
            .on("mouseout", resetChords)
            .text(function (d) {
              return d._id;
            });

        groups.select("path")
            .transition().duration(2000)
            .attrTween("d", matrix.groupTween(arc));

        groups.select("text")
            .transition()
            .duration(2000)
            .attr("transform", function (d) {
              d.angle = (d.startAngle + d.endAngle) / 2;
              var r = "rotate(" + (d.angle * 180 / Math.PI - 90) + ")";
              var t = " translate(" + (innerRadius + 26) + ")";
              return r + t + (d.angle > Math.PI ? " rotate(180)" : " rotate(0)");
            })
            .attr("text-anchor", function (d) {
              return d.angle > Math.PI ? "end" : "begin";
            });

        groups.exit().select("text").attr("fill", "orange");
        groups.exit().select("path").remove();

        groups.exit().transition().duration(1000)
            .style("opacity", 0).remove();

        var chords = container.selectAll("path.chord")
            .data(matrix.chords(), function (d) { return d._id; });

        chords.enter().append("path")
            .attr("class", "chord")
            .style("fill", function (d) {
              return colors(d.source._id);
            })
            .attr("d", path)
            .on("mouseover", chordMouseover)
            .on("mouseout", hideTooltip);

        chords.transition().duration(2000)
            .attrTween("d", matrix.chordTween(path));

        chords.exit().remove()

        function groupClick(d) {
          d3.event.preventDefault();
          d3.event.stopPropagation();
          $scope.addFilter(d._id);
          resetChords();
        }

        function chordMouseover(d) {
          d3.event.preventDefault();
          d3.event.stopPropagation();
          dimChords(d);
          d3.select("#tooltip").style("opacity", 1);
          $scope.updateTooltip(matrix.read(d));
        }

        function hideTooltip() {
          d3.event.preventDefault();
          d3.event.stopPropagation();
          d3.select("#tooltip").style("opacity", 0);
          resetChords();
        }

        function resetChords() {
          d3.event.preventDefault();
          d3.event.stopPropagation();
          container.selectAll("path.chord").style("opacity",0.9);
        }

        function dimChords(d) {
          d3.event.preventDefault();
          d3.event.stopPropagation();
          container.selectAll("path.chord").style("opacity", function (p) {
            if (d.source) { // COMPARE CHORD IDS
              return (p._id === d._id) ? 0.9: 0.1;
            } else { // COMPARE GROUP IDS
              return (p.source._id === d._id || p.target._id === d._id) ? 0.9: 0.1;
            }
          });
        }
      }; // END DRAWCHORDS FUNCTION

      function resize() {
        var width = $el.parent()[0].clientWidth;
        svg.attr({
          width: width,
          height: width / (size[0] / size[1])
        });
      }

      resize();

      $window.addEventListener("resize", function () {
        resize();
      });
    }; // END LINK FUNCTION

    return {
      link: link,
      restrict: 'EA'
    };

  }]);



