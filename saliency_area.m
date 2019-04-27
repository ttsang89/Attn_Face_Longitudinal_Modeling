% % COUNT UP THE FRAMES 

% CB_SN_maps=dir('charlieBrown/CB_salience/saliency/');
% % load(SN_maps);
% total_frames = length(CB_SN_maps)
% CB_sal=[];
% for frame =3:total_frames
%     thisFrameName=imread(CB_SN_maps(frame).name);
%     theFrameGray=rgb2gray(thisFrameName);
%         
%     thisFrameSal = length(find(theFrameGray(:,:)>55))/(length(theFrameGray(:,1))*length(theFrameGray(1,:)));
%     CB_sal=[thisFrameSal CB_sal];
% end
% 
% nanmean(CB_sal)*100
% 
% %    18.4177
% 
% 
% SST_SN_maps=dir('charlieBrown/SST_salience/saliency');
% % load(SN_maps);
% total_frames = length(SST_SN_maps)
% SST_sal=[];
% for frame =4:total_frames
%     thisFrameName=imread(SST_SN_maps(frame).name);
%     theFrameGray=rgb2gray(thisFrameName);
%         
%     thisFrameSal = length(find(theFrameGray(:,:)>55))/(length(theFrameGray(:,1))*length(theFrameGray(1,:)));
%     SST_sal=[thisFrameSal SST_sal];
% end
% 
% nanmean(SST_sal)*100

% 12.9962


points_file = {'charlieBrown/CharlieBrown_FA.mat','charlieBrown/SS_FA.mat' };
% CB movie size = [704 472];
% SST movie = [720 480];
 smooth_rad=20;
 moviesize=[332288 345600]
for cond = 1:2;
    load(points_file{cond});
    total_face=[];
    for c = 1:length(points)
        
      for p = 1:length(points(c).p1s)
%     
          x1 = points(c).p1s{p}(2) - smooth_rad;
          x2 = points(c).p2s{p}(2) + smooth_rad;
          y1 = points(c).p1s{p}(1) - smooth_rad;
          y2 = points(c).p2s{p}(1) + smooth_rad;
          
          face_area(p) = (x2-x1).*(y2-y1);
          
      end
      total_face = [total_face sum(face_area)/moviesize(cond)];
      
    end
    this_clip_face_area(cond)=mean(total_face);
    this_clip_face_SD(cond)=std(total_face);
    face_area_total{cond}=total_face';
end

CB_movie_size=704*472
CB_face_area= this_clip_face_area(1)*100
CB_STDEV=this_clip_face_SD(1)*100

%    37.4202
SST_movie_size=720*480
SST_face_area=this_clip_face_area(2)*100
SST_STDEV=this_clip_face_SD(2)*100
%    39.0105

CBface=face_area_total{1};
SSTface=face_area_total{2};
[h,p,ci,stats] = ttest2(SSTface, CBface, 'Tail', 'both')

ave_p= [];
ave_ci=[];
for i = 1:1000
   
cb_random = datasample(CBface, 1000);
sst_random = datasample(SSTface,1000);

[h,thisp,thisci,stats] = ttest2(cb_random, sst_random, 'Tail', 'both');

ave_p = [ave_p thisp];
ave_ci=[ave_ci thisci];

clear cb_random sst_random
end


[ci]=bootci(1000,@mean, ave_p)



    
      
      